;;; retrospect.el --- Retrospective mode for org.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Nicolas Dudebout

;; Author: Nicolas Dudebout <nicolas.dudebout@gmail.com>
;; Maintainer: Nicolas Dudebout <nicolas.dudebout@gmail.com>
;; Created: 23 Feb 2018
;; Modified: 31 Aug 2019
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.2") (dash "2.14"))
;; Keywords: org-mode retrospective time-management
;; URL: https://github.com/dudebout/retrospect

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The package-level documentation is available in the repository's README.md.

(require 'dash)
(require 'org)

;;; Code:

(defcustom retrospect-buffer-name "*retrospect*"
  "The name of the output buffer."
  :type 'string
  :group 'retrospect)

(defcustom retrospect-source-filename nil
  "The input file."
  :type 'string
  :group 'retrospect)

(defcustom retrospect-buckets nil
  "Definition of the buckets.

Each bucket is identified by a symbol.

`:names' is an association list whose car are symbols identifying
buckets and cdr their human-friendly names, used in the
*retrospect* buffer.

`:classifier' is a function which returns the bucket-identifying
symbol for the org entry at point."
  :type '(plist :options ((:names (alist :key-type symbol :value-type string))
                          (:classifier function)))
  :group 'retrospect)

(defcustom retrospect-time-range nil
  "Time range to be analyzed."
  :type '(plist :options ((:tstart string)
                          (:tend string)))
  :group 'retrospect)

(defcustom retrospect-summary-transfers nil
  "Rules to transfer bucket contents for the summary.

Alist whose keys are buckets to be emptied, and values are alists
defining in which bucket the content should be transferred.  Each
such alist defines a weighted list of buckets, where keys are
buckets, and values are weights."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type number))
  :group 'retrospect)

(defcustom retrospect-bucket-property-name "bucket"
  "Org property identifying the bucket of an org entry.

Interning the value provides a bucket-identifying symbol."
  :type 'string
  :group 'retrospect)

(defcustom retrospect-insert-org-links t
  "If t insert links to the org entries, else only display their names."
  :type 'boolean
  :group 'retrospect)

(defcustom retrospect-display-empty-buckets nil
  "If t insert information for bucket with no associated logged time."
  :type 'boolean
  :group 'retrospect)

(defcustom retrospect-display-summary nil
  "If t display a summary in the *retrospect* buffer."
  :type 'boolean
  :group 'retrospect)

(defcustom retrospect-display-details t
  "If t display a table with detailled buckets content in the *retrospect* buffer."
  :type 'boolean
  :group 'retrospect)

(defcustom retrospect-minutes-fmt 'duration
  "Format to display logged minutes.

When set to `duration', the minutes are displayed as absolute
durations.  When set to `percentage' they are displayed as
percentages of the total time logged."
  :type '(choice (const duration)
                 (const percentage))
  :group 'retrospect)

(defcustom retrospect-indent-str "    "
  "String used to indent org entries in the *retrospect* buffer."
  :type 'string
  :group 'retrospect)

(defvar retrospect-total-durations nil
  "Alist to accumulate the time logged per bucket.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Data manipulation
;;

(defun retrospect--compute-logged-durations (&optional tstart tend)
  "Compute the durations logged on each org entry between TSTART and TEND.

The results are stored as text properties on the input file."
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(:retrospect-clock-minutes t)))
  (setq retrospect-total-durations nil)

  (let* ((buckets ())
        (classifier (plist-get retrospect-buckets :classifier))
        ;; Use the predicate's side effect to compute the list of buckets
        ;; against which time was loggend between `tstart' and `tend'.
        (pred (lambda ()
                (setq buckets (-union buckets (list (funcall classifier))))
                nil)))
    (org-clock-sum tstart tend pred)

    ;; Traversing the bucket once per-bucket, so that `org-clock-sum' takes care
    ;; of assigning time logged to the parent of entries in a given bucket.
    (dolist (bucket buckets)
      (let* ((pred (lambda ()
                     (equal (funcall classifier) bucket))))
        (org-clock-sum tstart tend pred)
        (with-silent-modifications
          (save-excursion
            (goto-char (point-min))
            (while
                (progn
                  ;; `minutes-new' and `minutes-acc' contain data relative to the
                  ;; org entry at point:
                  ;;   + `minutes-new' is the time logged against `bucket'
                  ;;   + `minutes-acc' is an alist with the time logged against all
                  ;;     the buckets processed thus far
                  (let ((minutes-new (get-text-property (point) :org-clock-minutes))
                        (minutes-acc (get-text-property (point) :retrospect-clock-minutes)))
                    (when minutes-new
                      (when (equal (org-current-level) 1)
                        (cl-incf (alist-get bucket retrospect-total-durations 0) minutes-new))
                      (cl-incf (alist-get bucket minutes-acc 0) minutes-new)
                      (put-text-property (point) (point-at-eol) :retrospect-clock-minutes minutes-acc)))
                  (outline-next-heading)))))))))

(defun retrospect--total-minutes (durations)
  "Return the total logged time in the DURATIONS alist."
  (apply #'+ (mapcar #'cdr durations)))

(defun retrospect--transferred-durations ()
  "Return an alist of per-bucket durations to be displayed in the summary."
  (let ((result (copy-alist retrospect-total-durations)))
    (dolist (src (mapcar #'car retrospect-summary-transfers))
      (let ((transferred-minutes (alist-get src result))
            (denominator (apply #'+ (mapcar #'cdr (alist-get src retrospect-summary-transfers)))))
        (dolist (dst (mapcar #'car (alist-get src retrospect-summary-transfers)))
          (let* ((numerator (alist-get dst (alist-get src retrospect-summary-transfers)))
                 (ratio (/ (float numerator) denominator))
                 (minutes (* transferred-minutes ratio)))
            (cl-incf (alist-get dst result 0) minutes)))
        (setf result (cl-delete src result :key #'car :test #'equal))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Display
;;

(defun retrospect--minutes-str (durations minutes)
  "Pretty print the time duration MINUTES in hours and minutes, or percentages of the time logged in DURATIONS."
  (pcase retrospect-minutes-fmt
    ('percentage
     (format "%.1f%%" (/ (* minutes 100.0) (retrospect--total-minutes durations))))
    ('duration
     (format-seconds "%h:%02m" (* 60 minutes)))))

(defun retrospect--insert-buckets-content ()
  "Insert each org entry with its duration under its containing bucket."
  (let ((unnamed-buckets (-difference
                          (mapcar #'car retrospect-total-durations)
                          (mapcar #'car (plist-get retrospect-buckets :names)))))
    (when unnamed-buckets
      (insert "* Errors\n")
      (dolist (bucket unnamed-buckets)
        (let ((bucket-minutes (alist-get bucket retrospect-total-durations)))
          (retrospect--insert-details-header (symbol-name bucket) bucket-minutes))
          (retrospect--insert-details-body bucket))
      (retrospect--finalize-details)))
  (when retrospect-display-summary
    (insert "* Summary\n")
    (let ((transferred-durations (retrospect--transferred-durations)))
      (dolist (bucket (mapcar #'car (plist-get retrospect-buckets :names)))
        (let ((bucket-minutes (alist-get bucket transferred-durations))
              (retrospect-minutes-fmt 'percentage))
          (when (or bucket-minutes retrospect-display-empty-buckets)
            (insert (format "+ %s :: %s\n"
                            (alist-get bucket (plist-get retrospect-buckets :names))
                            (retrospect--minutes-str transferred-durations (or bucket-minutes 0)))))))))
  (when retrospect-display-details
    (insert "* Details\n")
    (dolist (bucket (mapcar #'car (plist-get retrospect-buckets :names)))
      (let ((bucket-minutes (alist-get bucket retrospect-total-durations)))
        (when (or bucket-minutes retrospect-display-empty-buckets)
          (retrospect--insert-details-header (alist-get bucket (plist-get retrospect-buckets :names)) (or bucket-minutes 0))))
          (retrospect--insert-details-body bucket))
    (retrospect--finalize-details))
  (retrospect--finalize-buffer))

(defun retrospect--insert-details-header (name minutes)
  "As part of a details table, insert the org table header for a section.

The section is titled NAME, and has MINUTES logged against it."
  (insert "|-|\n")
  (insert (format "|%s|%s|\n|-|\n"
                  name
                  (retrospect--minutes-str retrospect-total-durations minutes))))

(defun retrospect--insert-details-body (bucket)
  "As part of a details table, insert the org table body BUCKET's section."
  (with-current-buffer (find-file-noselect retrospect-source-filename)
    (save-excursion
      (goto-char (point-min))
      (while
          (progn
            (let ((level (org-current-level))
                  (heading
                   (if retrospect-insert-org-links
                       (org-store-link nil)
                     (org-element-property :raw-value (org-element-at-point))))
                  (minutes (alist-get bucket (get-text-property (point) :retrospect-clock-minutes))))
              (when minutes
                (with-current-buffer retrospect-buffer-name
                  (insert "|")
                  ;; the following could make use of s-replace
                  (dotimes (_ (- level 1))
                    (insert (replace-regexp-in-string (regexp-quote " ") " " retrospect-indent-str)))
                  (insert heading)
                  (dotimes (_ level) (insert "|"))
                  (insert (retrospect--minutes-str retrospect-total-durations minutes))
                  (insert "|\n")))
              (outline-next-heading)))))))

(defun retrospect--finalize-details ()
  "Finalize a details table."
  (insert "|-|\n")
  (org-table-align))

(defun retrospect--finalize-buffer ()
  "Finalize the *retrospect* buffer."
  (goto-char (point-min))
  (while (search-forward " " nil t)
    (replace-match " ")))

(defun retrospect--refresh-buffer ()
  "Compute and display buckets content.

This function is intended to be called from the *retrospect*
buffer setup by a call to `retrospect'."
  (interactive)
  (retrospect--compute-buckets-content)
  (retrospect--redraw-buffer))

(defun retrospect--compute-buckets-content ()
  "Compute buckets content."
  (with-current-buffer (find-file-noselect retrospect-source-filename)
    (retrospect--compute-logged-durations
     (plist-get retrospect-time-range :tstart)
     (plist-get retrospect-time-range :tend))))

(defun retrospect--redraw-buffer ()
  "Erase the *retrospect* buffer and display buckets content."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (retrospect--insert-buckets-content))
  (goto-char (point-min)))

(defun retrospect--cycle-minutes-fmt ()
  "Cycle through `retrospect-minutes-fmt' and redraw the *retrospect* buffer."
  (interactive)
  (setq retrospect-minutes-fmt
        (if (equal retrospect-minutes-fmt 'duration)
            'percentage
          'duration))
  (retrospect--redraw-buffer))

(defun retrospect--toggle-org-links ()
  "Toggle `retrospect-insert-org-links' and redraw the *retrospect* buffer."
  (interactive)
  (setq retrospect-insert-org-links (not retrospect-insert-org-links))
  (retrospect--redraw-buffer))

(defun retrospect--toggle-empty-buckets ()
  "Toggle `retrospect-display-empty-buckets' and redraw the *retrospect* buffer."
  (interactive)
  (setq retrospect-display-empty-buckets (not retrospect-display-empty-buckets))
  (retrospect--redraw-buffer))

(defun retrospect--toggle-summary ()
  "Toggle `retrospect-display-summary' and redraw the *retrospect* buffer."
  (interactive)
  (setq retrospect-display-summary (not retrospect-display-summary))
  (retrospect--redraw-buffer))

(defun retrospect--toggle-details ()
  "Toggle `retrospect-display-details' and redraw the *retrospect* buffer."
  (interactive)
  (setq retrospect-display-details (not retrospect-display-details))
  (retrospect--redraw-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; User Interface
;;

(defun retrospect ()
  "Setup the `retrospect' buffer, compute, and display buckets content."
  (interactive)
  (dolist (var '(retrospect-source-filename retrospect-buckets))
    (unless (symbol-value var)
      (error (format "`%s' is not set" (symbol-name var)))))
  (let ((buffer (get-buffer-create retrospect-buffer-name)))
    (with-current-buffer buffer
      (org-mode)
      (retrospect-mode 1)
      (retrospect--refresh-buffer))
    (switch-to-buffer buffer)))

(define-minor-mode retrospect-mode
  "Retrospective mode for org.

Determine how time you logged in org was spent. Use this
information to bill clients, or understand where you spend your
time, etc. `retrospect` categorizes org entries with logged time
into buckets, which are defined in the `retrospect-buckets`
variable, and displays a summary in the *retrospect* buffer."
  :lighter " Retro"
  :keymap `(("g" . retrospect--refresh-buffer)
            ("f" . retrospect--cycle-minutes-fmt)
            ("l" . retrospect--toggle-org-links)
            ("e" . retrospect--toggle-empty-buckets)
            ("s" . retrospect--toggle-summary)
            ("d" . retrospect--toggle-details)
            ("q" . bury-buffer)
            ("n" . org-next-link)
            ("p" . org-previous-link)
            (,(kbd "<return>") . org-open-at-point))
  :group 'retrospect
  (setq buffer-read-only t))

(defun retrospect-bucket-from-property ()
  "Determine the bucket of the org entry at point from its `bucket` property.

The result is a bucket symbol.

If the entry at point does not have a `bucket` property, return nil."
  (let ((bucket-property (org-entry-get (point) retrospect-bucket-property-name t)))
    (when bucket-property
        (intern bucket-property))))

(provide 'retrospect)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; retrospect.el ends here
