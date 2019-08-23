;;; retrospect.el --- Retrospective mode for org.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Nicolas Dudebout

;; Author: Nicolas Dudebout <nicolas.dudebout@gmail.com>
;; Maintainer: Nicolas Dudebout <nicolas.dudebout@gmail.com>
;; Created: 23 Feb 2018
;; Modified: 05 Aug 2019
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.2"))
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

`:names' is a list of cons cells whose car are symbols
identifying buckets and cdr their human-friendly names, used in
the *retrospect* buffer.

`:classifier' is a function which returns the bucket-identifying
symbol for the org entry at point."
  :type '(plist :options ((:names (repeat (cons symbol string)))
                          (:classifier function)))
  :group 'retrospect)

(defcustom retrospect-time-range nil
  "Time range to be analyzed."
  :type '(plist :options ((:tstart string)
                          (:tend string)))
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
  "If t insert headers for bucket with no associated logged time."
  :type 'boolean
  :group 'retrospect)

(defcustom retrospect-indent-str "    "
  "String used to indent org entries in the *retrospect* buffer."
  :type 'string
  :group 'retrospect)

(defvar retrospect-total-clock-minutes nil
  "Plist to accumulate the time logged per bucket.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Data manipulation
;;

(defun retrospect--compute-logged-durations (&optional tstart tend)
  "Compute the durations logged on each org entry between TSTART and TEND.

The results are stored as text properties on the input file."
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(:retrospect-clock-minutes t)))
  (setq retrospect-total-clock-minutes nil)
  (dolist (bucket (mapcar #'car (plist-get retrospect-buckets :names)))
    (let* ((classifier (plist-get retrospect-buckets :classifier))
           (pred (lambda ()
                   (equal (funcall classifier) bucket))))
      ;; actual computation
      (org-clock-sum tstart tend pred)
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (while
              (progn
                ;; `minutes-new' and `minutes-acc' contain data relative to the
                ;; org entry at point:
                ;;   + `minutes-new' is the time logged against `bucket'
                ;;   + `minutes-acc' is a plist with the time logged against all
                ;;     the buckets processed thus far
                (let ((minutes-new (get-text-property (point) :org-clock-minutes))
                      (minutes-acc (get-text-property (point) :retrospect-clock-minutes)))
                  (when minutes-new
                    (when (equal (org-current-level) 1)
                      (setq retrospect-total-clock-minutes (plist-put retrospect-total-clock-minutes bucket (+ minutes-new
                                                                                                               (or (plist-get retrospect-total-clock-minutes bucket) 0)))))
                    (let ((minutes-acc (plist-put minutes-acc bucket (+ minutes-new
                                                                        (or (plist-get minutes-acc bucket) 0)))))
                      (put-text-property (point) (point-at-eol) :retrospect-clock-minutes minutes-acc))))
                (outline-next-heading))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Display
;;

(defun retrospect--minutes-str (minutes)
  "Pretty print the time duration MINUTES in hours and minutes."
  (format-seconds "%h:%02m" (* 60 minutes)))

(defun retrospect--insert-buckets-content ()
  "Insert each org entry with its duration under its containing bucket."
  (dolist (bucket (mapcar #'car (plist-get retrospect-buckets :names)))
    (let ((bucket-minutes (plist-get retrospect-total-clock-minutes bucket)))
      (when (or bucket-minutes retrospect-display-empty-buckets)
        (insert "|-|\n")
        (insert (format "|%s|%s|\n|-|\n"
                        (alist-get bucket (plist-get retrospect-buckets :names))
                        (retrospect--minutes-str (or bucket-minutes 0)))))
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
                      (minutes (plist-get (get-text-property (point) :retrospect-clock-minutes) bucket)))
                  (when minutes
                    (with-current-buffer retrospect-buffer-name
                      (insert "|")
                      ;; the following could make use of s-replace
                      (dotimes (_ (- level 1))
                        (insert (replace-regexp-in-string (regexp-quote " ") " " retrospect-indent-str)))
                      (insert heading)
                      (dotimes (_ level) (insert "|"))
                      (insert (retrospect--minutes-str minutes))
                      (insert "|\n")))
                  (outline-next-heading))))))))
  (insert "|-|\n")
  (org-table-align)
  (goto-char (point-min))
  (while (search-forward " " nil t)
    (replace-match " ")))

(defun retrospect--refresh-buffer ()
  "Compute and display buckets content.

This function is intended to be called from the *retrospect*
buffer setup by a call to `retrospect'."
  (interactive)
  (retrospect--compute-buckets-content)
  (retrospect--populate-buffer))

(defun retrospect--compute-buckets-content ()
  "Compute buckets content."
  (with-current-buffer (find-file-noselect retrospect-source-filename)
    (retrospect--compute-logged-durations
     (plist-get retrospect-time-range :tstart)
     (plist-get retrospect-time-range :tend))))

(defun retrospect--populate-buffer ()
  "Display buckets content."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (retrospect--insert-buckets-content))
  (goto-char (point-min)))

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
