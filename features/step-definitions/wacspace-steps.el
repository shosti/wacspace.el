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
    (should (equal frame-alignment alignment))))

(And "^the \\([0-9]+\\)\\(st\\|nd\\|rd\\|th\\) window should be in buffer \"\\([^\"]+\\)\"$"
  (lambda (n _ expected-name)
    (save-excursion
      (select-window (window-at 1 1))
      (should (equal (buffer-name
                      (window-buffer
                       (nth (- (string-to-int "2") 1) (window-list))))
                     expected-name)))))

(Then "^there should be \\([0-9]+\\) windows$"
  (lambda (wins)
    (should (= (string-to-int wins)
               (length (window-list))))))
