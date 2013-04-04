;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)

;; Public configuration options

(defvar wacs-fill-screen-fn nil
  "Function to fill screen.")
(defvar wacs-left-screen-fn nil
  "Function to fill the left half of the screen.")
(defvar wacs-right-screen-fn nil
  "Function to fill the right half of the screen.")
(defvar wacs-top-screen-fn nil
  "Function to fill the top of the screen.")
(defvar wacs-bottom-screen-fn nil
  "Function to fill the bottom of the screen.")

;; Private configuration

(defvar wacs--config nil)
(defvar wacs--winconfs nil)
(defconst wacs--numeric-confs '(:default :1 :2 :3 :4 :5 :6 :7 :8 :9))

;; Helper functions and macros

(cl-defmacro wacs--when-let ((var value) &rest body)
  `(let ((,var ,value))
     (when ,var ,@body)))

(defun wacs--eval-aux-cond (aux-cond)
  (cond ((listp aux-cond)
         (funcall aux-cond))
        ((boundp aux-cond)
         (eval aux-cond))))

;; Configuration

(defun wacs--get-config (&optional arg)
  (defun resolve-config (config)
    (let ((arg-key (if arg
                       (intern (concat ":" arg))
                     :default)))
      (append (--filter (not (memq (car it) wacs--numeric-confs))
                        config)
              (cdr (assq arg-key config)))))

  (let* ((mode-config-list (cdr (assoc major-mode wacs--config)))
         (config
          (cl-dolist (aux-cond-pair mode-config-list)
            (unless (eq (car aux-cond-pair) :default)
              (when (wacs--eval-aux-cond (car aux-cond-pair))
                (cl-return (cdr aux-cond-pair)))))))
    (resolve-config (if config config
                      (cdr (assq :default mode-config-list))))))

(cl-defmacro defwacspace ((mode &optional aux-cond) &body configuration)
  "Define a wacspace for a major mode and an optional auxiliary
condition. The auxiliary condition can either be a variable (such
as a minor mode) or a lambda. For full documentation of
configuration options, see the README."
  (defun list->dotted-pair (list)
    (cons (car list) (cadr list)))

  (defun process-config (config)
    (let ((default-conf (-map 'list->dotted-pair
                              (cdr (assq :default config)))))
      (unless default-conf
        (error
         "Please include a :default configuration for the wacspace"))
      (-map
       (lambda (entry)
         (cond ((eq (car entry) :default)
                (cons :default default-conf))
               ((memq (car entry) wacs--numeric-confs)
                (append (cons (car entry)
                              (-map 'list->dotted-pair (cdr entry)))
                        default-conf))
               (t (list->dotted-pair entry))))
       config)))

  (let* ((config (process-config configuration))
         (aux-cond-key (or aux-cond :default))
         (config-entry `(,aux-cond-key . ,config))
         (mode-list-pair-var (cl-gensym)))
    `(let ((,mode-list-pair-var (assq ',mode wacs--config)))
       (if ,mode-list-pair-var
           (push ',config-entry
                 (cdr ,mode-list-pair-var))
         (push (cons ',mode (list ',config-entry))
               wacs--config))
       t)))

(cl-defmacro defwinconf ((conf-name) &body body)
  `(push (cons ',conf-name
               '(lambda () ,@body))
         wacs--winconfs))



;; (defun wacspace (&optional arg)
;;   (interactive "P")
;;   (wacs--when-let ((config (wacs--get-config arg)))))

;; Standard configuration

(defwinconf (3win)
  (split-window-right)
  (other-window)
  (split-window-below))

(provide 'wacspace)
