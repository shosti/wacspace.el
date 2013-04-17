;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(And "^I close all the other windows$"
  (lambda ()
    (delete-other-windows)))

(And "^I run \\(.+\\)$"
  (lambda (f)
    (funcall (intern f))))

(And "^I turn off \\(.+\\)$"
  (lambda (mode)
    (funcall (intern mode) '0)))

(And "^the frame should be \\([a-z]+\\)\\( aligned\\)?"
  (lambda (alignment _)
    (assert (equal frame-alignment alignment) nil
            "Frame should be %s aligned but is %s aligned" alignment frame-alignment)))

(And "^the \\([0-9]+\\)\\(st\\|nd\\|rd\\|th\\) window should be in buffer \"\\([^\"]+\\)\"$"
  (lambda (n _ expected-name)
    (save-excursion
      (select-window (window-at 1 1))
      (other-window (- (string-to-number n) 1))
      (assert (equal expected-name (buffer-name)) nil
              "Window #%s should be in buffer %s but is in buffer %s"
              n expected-name (buffer-name)))))

(Then "^there should be \\([0-9]+\\) windows$"
  (lambda (wins)
    (assert (= (string-to-number wins)
               (length (window-list))) nil
            "There are %s windows when there should be %s"
            (length (window-list)) wins)))

(And "^I kill the current buffer$"
       (lambda ()
         (let ((kill-buffer-query-functions nil))
           (kill-buffer (current-buffer)))))

(And "^I rename the buffer to \"\\([^\"]+\\)\"$"
       (lambda (name)
         (rename-buffer name)))

(And "^I visit the file \"\\([^\"]+\\)\"$"
       (lambda (fname)
         (find-file (concat base-dir "/" fname))))

(And "^the current directory should be the base directory$"
       (lambda ()
         (assert (equal default-directory base-dir) nil
                 "Current directory should equal %s but instead is %s"
                 base-dir default-directory)))

(And "^I save the buffer$"
       (lambda ()
         (save-buffer)))

(And "^I create the directory \"\\([^\"]+\\)\"$"
       (lambda (dirname)
         (make-directory (concat base-dir dirname))))

(Then "^I should be in \\([-a-z]+-mode\\)$"
       (lambda (mode-string)
         (assert (equal major-mode (intern mode-string)) nil
                 "Mode should be %s but is %s" major-mode mode-string)))

(Then "^I should be in a buffer like \"\\([^\"]+\\)\"$"
  (lambda (expected-name)
    (string-match expected-name (buffer-name))))
