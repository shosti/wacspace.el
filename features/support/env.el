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
 (defvar frame-alignment nil)
 (wacs-set-frame-fn full
                    (lambda ()
                      (setq frame-alignment "full")))
 (wacs-set-frame-fn right
                    (lambda ()
                      (setq frame-alignment "right")))
 (wacs-set-frame-fn left
                    (lambda ()
                      (setq frame-alignment "left")))
 (wacs-set-frame-fn top
                    (lambda ()
                      (setq frame-alignment "top")))
 (wacs-set-frame-fn bottom
                    (lambda ()
                      (setq frame-alignment "bottom")))

 (get-buffer-create "*main*")
 (global-set-key (kbd "C-c w") 'wacspace)
 (global-set-key (kbd "C-c s") 'wacspace-save))

(Before
 (setq base-dir (concat (make-temp-file "wacs" t) "/"))
 (setq wacs--config nil)
 (setq wacs--saved-workspaces (make-hash-table :test 'equal))
 (jump-to-register :pre-ecukes))

(After
 (when (-contains? (-map 'buffer-name (buffer-list)) "*eshell*")
   (kill-buffer "*eshell*")))

(Teardown
 ;;
 )
