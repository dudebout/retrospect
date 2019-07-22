;;; retrospect-tests.el --- Tests for retrospect.el

;;; Commentary:

;;; Code:

(require 'ert)
(require 'retrospect)

(defun has-bucket-property (bucket-name)
  "Return t if the entry at point has the `bucket` property BUCKET-NAME."
  (equal (org-entry-get (point) "bucket" t)
         bucket-name))

(defun test-file (file)
  "Locate test file named FILE."
  (format "%s/test/%s" (getenv "PWD") file))

(defun test-file-with-suffix (file suffix)
  "Locate test file named FILE.SUFFIX."
  (test-file (format "%s.%s" file suffix)))

(defun test-file-org (file)
  "Locate test file named FILE.org."
  (test-file-with-suffix file "org"))

(defun test-file-golden (file)
  "Locate test file named FILE.golden."
  (test-file-with-suffix file "golden"))

(defun run-golden-test (name buckets)
  "Run `retrospect' and compare the result with a golden output.

Use buckets defined in BUCKETS, on test file NAME.org, and
compare with golden output test file NAME.golden."
  (setq retrospect-insert-org-links nil
        retrospect-source-filename (test-file-org name)
        retrospect-buckets buckets)
  (retrospect)
  (let ((expected (progn
                    (find-file (test-file-golden name))
                    (buffer-string)))
        (got (with-current-buffer retrospect-buffer-name
               (buffer-string))))
    (should (equal got expected))))

(defun run-interactive (name buckets)
  "Run `retrospect' in interactive mode.

Use buckets defined in BUCKETS, on test file NAME.org."
  (setq visible-bell t
        debug-on-error t)
  (setq retrospect-insert-org-links t
        retrospect-source-filename (test-file-org name)
        retrospect-buckets buckets)
  (global-set-key (kbd "C-c t") #'retrospect)
  (retrospect))

(defvar end-to-end-buckets
  '(:names
    ((bucket-a . "Bucket A")
     (bucket-b . "Bucket B")
     (bucket-c . "Bucket C"))
    :classifiers
    (((lambda () (has-bucket-property "a")) . bucket-a)
     ((lambda () (has-bucket-property "b")) . bucket-b)
     ((lambda () (has-bucket-property "c")) . bucket-c))))

(ert-deftest end-to-end-test ()
  (run-golden-test "end-to-end" end-to-end-buckets))

;;; retrospect-tests.el ends here
