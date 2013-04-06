;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq wacspace-root-path project-directory)
  (setq wacspace-util-path (expand-file-name "util" wacspace-root-path)))

(add-to-list 'load-path wacspace-root-path)
(add-to-list 'load-path (expand-file-name "espuds" wacspace-util-path))
(add-to-list 'load-path (expand-file-name "ert" wacspace-util-path))

(require 'wacspace)
(require 'espuds)
(require 'ert)
(require 'ruby-mode)
(require 'rinari)


(Setup
 (window-configuration-to-register :pre-ecukes)
 (get-buffer-create "*main*")
 (global-set-key (kbd "C-c C-w") 'wacspace)
 )

(Before
 (jump-to-register :pre-ecukes)
 )

(After
 ;; After each scenario is run
 )

(Teardown
;;
 )
