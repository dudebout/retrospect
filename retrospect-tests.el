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

(defun run-golden-test (input output)
  "Run `retrospect' on test file INPUT.org and compare the result with OUTPUT.golden."
  (let ((retrospect-insert-org-links nil)
        (retrospect-source-filename (test-file-org input)))
    (retrospect)
    (let ((expected (progn
                      (find-file (test-file-golden output))
                      (buffer-string)))
          (got (with-current-buffer retrospect-buffer-name
                 (buffer-string))))
      (should (equal got expected)))))

(defvar default-buckets
  '(:names
    ((a . "Bucket A")
     (b . "Bucket B")
     (c . "Bucket C"))
    :classifier
    retrospect-bucket-from-property))

(defvar default-time-range
  '(:tstart "2018-12-29 Sat 13:02"
    :tend "2018-12-29 Sat 13:03"))

(ert-deftest default-test ()
  (let ((retrospect-buckets default-buckets))
    (run-golden-test "input" "default")))

(ert-deftest timed-test ()
  (let ((retrospect-buckets default-buckets)
        (retrospect-time-range default-time-range))
    (run-golden-test "input" "timed")))

(defun run-interactive (input)
  "Run `retrospect' in interactive mode on test file INPUT.org."
  (setq visible-bell t
        debug-on-error t)
  (setq retrospect-insert-org-links t
        retrospect-source-filename (test-file-org input))
  (global-set-key (kbd "C-c t") #'retrospect)
  (retrospect))

(defvar demo-buckets default-buckets)

;;; retrospect-tests.el ends here
