;;; retrospect-tests.el --- Tests for retrospect.el

;;; Commentary:

;;; Code:

(require 'ert)
(require 'retrospect)

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

(defun run-golden-test (input buckets output)
  "Run `retrospect' and compare the result with a golden output.

Use buckets defined in BUCKETS, on test file INPUT.org, and
compare with golden output test file OUTPUT.golden."
  (setq retrospect-insert-org-links nil
        retrospect-source-filename (test-file-org input)
        retrospect-buckets buckets)
  (retrospect)
  (let ((expected (progn
                    (find-file (test-file-golden output))
                    (buffer-string)))
        (got (with-current-buffer retrospect-buffer-name
               (buffer-string))))
    (should (equal got expected))))

(defun run-interactive (input buckets)
  "Run `retrospect' in interactive mode.

Use buckets defined in BUCKETS, on test file NAME.org."
  (setq visible-bell t
        debug-on-error t)
  (setq retrospect-insert-org-links t
        retrospect-source-filename (test-file-org input)
        retrospect-buckets buckets)
  (global-set-key (kbd "C-c t") #'retrospect)
  (retrospect))

(defvar default-buckets
  '(:names
    ((a . "Bucket A")
     (b . "Bucket B")
     (c . "Bucket C"))
    :classifier
    retrospect-bucket-from-property))

(ert-deftest default-test ()
  (run-golden-test "input" default-buckets "default"))

(defvar demo-buckets default-buckets)

;;; retrospect-tests.el ends here
