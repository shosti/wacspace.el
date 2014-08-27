;;; wacs-configuration.el --- Configuration for wacspace -*- lexical-binding: t -*-

;; Copyright © 2013-2014 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration management for wacspace.el.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'wacs-util)

(defgroup wacspace nil
  "The WACky WorkSPACE manager for emACS"
  :prefix "wacs-"
  :group 'environment)

(defcustom wacs-regexp-buffer-switching t
  "Use regexp matching for buffer switching in `wacspace'.

When set to t, :buffer option will use a regexp match if a
buffer does not exist with the exact match."
  :group 'wacspace
  :type 'boolean)

(defcustom wacs-save-frame (display-graphic-p)
  "Save frame with `wacspace'.

When set to t, wacspace will save the frame configuration as
well as the window configuration.  Set to t by default in graphic
display and nil if Emacs is run in a terminal."
  :group 'wacspace
  :type 'boolean)

(defcustom wacs-project-base-file ".git"
  "Default base file name in projects.

Wacspace will assume that project base directories have this
filename in them.  This variable be dynamically bound within
helper functions.  When set to nil, wacspace will assume that the
current directory is the base directory."
  :group 'wacspace
  :type 'string)

(defvar wacs--winconfs nil
  "The wacspace winconf alist.

Should not be altered manually—use `defwinconf' instead.")

(defvar wacs-main-buffer nil
  "The buffer from which wacspace was called.

Should not be set directly; will be automatically bound when
wacspace is called.")

;; Because it's used within autoloaded macros, wacs--frame-fns needs to
;; be autoloaded

;;;###autoload
(defvar wacs--frame-fns nil
  "The wacspace frame function alist.

Should not be altered manually—use `wacs-set-frame-fn' instead.")

(defvar wacs--saved-workspaces (make-hash-table :test 'equal)
  "The hash table of saved workspaces.

Should not be altered manually—use `wacspace-save' instead.")

(defconst wacs--numeric-confs '(:default :1 :2 :3 :4 :5 :6 :7 :8 :9)
  "The numeric prefix configurations available to `wacspace'.")

(defvar wacs--project-name-fn nil
  "Function to determine the current project's name.

Should not be altered manually—use the :project-name-fn option
instead.")

(defvar wacs--open-projects nil
  "Alist with configuration for currently open projects.")

(defvar wacs--after-switch-fns (make-hash-table :test 'equal)
  "Hash table of functions to call before switching to projects.

Keys are project names, values are functions.")

(defvar wacs--persistent-local-vars
  '(wacs-project-base-file
    wacs--project-name-fn)
  "Variables that will become buffer-local.")

(defun wacs-project-dir ()
  "Return the project directory of `wacs-main-buffer'.

Looks for `wacs-project-base-file'.  If not found, defaults to the
current directory."
  (-if-let (dir (buffer-file-name wacs-main-buffer))
      (let ((fname (file-name-directory dir)))
        (expand-file-name
         (-if-let* ((base-file wacs-project-base-file)
                    (project-dir (locate-dominating-file
                                  fname
                                  base-file)))
             project-dir
           (file-name-directory fname))))
    default-directory))

(defun wacs-project-name ()
  "Return the name of the current project."
  (if wacs--project-name-fn
      (funcall wacs--project-name-fn)
    (-> (wacs-project-dir)
      (split-string "/" t)
      (last)
      (car))))

(defun wacs--resolve-prefix (config arg)
  "Return the final configuration from CONFIG with prefix ARG."
  (let ((arg-key (if arg
                     (intern (concat ":" (number-to-string arg)))
                   :default)))
    (append (--filter (not (memq (car it) wacs--numeric-confs))
                      config)
            (wacs--alist-get arg-key config)
            (wacs--alist-get arg-key
                             (wacs--alist-get :default
                                              (get :default
                                                   'wacs-config))))))

(defun wacs--get-cond-config-from-alist (config-alist)
  "Get the first first configuration with a satisfied auxiliary condition from CONFIG-ALIST."
  (cl-dolist (aux-cond-pair config-alist)
    (unless (eq (car aux-cond-pair) :default)
      (when (wacs--eval-aux-cond (car aux-cond-pair))
        (cl-return (cdr aux-cond-pair))))))

(defun wacs--get-default-config-from-alist (config-alist)
  "Get the :default configuration for the current major mode from CONFIG-ALIST."
  (wacs--alist-get :default config-alist))

(defun wacs--get-aliased-config (entry)
  "Get the configuration pointed to by alias entry ENTRY."
  (wacs--alist-get (or (cdr entry) :default)
                   (get (car entry) 'wacs-config)))

(defun wacs--get-config (&optional arg)
  "Get the config with prefix ARG associated with the current buffer.

First, search for a wacspace configuration with a satisfactory
auxiliary condition. Then, search for an alias with a
satisfactory auxiliary condition. Then, search for a
configuration without an auxiliary condition. Then, search for an
alias without an auxiliary condition. Then, search for the
default configuration with an auxiliary condition. Then, search
for the default configuration. Then give up. Whew."
  (let (mode-config mode-alias global-default-config)
    (let ((config
           (cl-block find-config
             (setq mode-config (get major-mode 'wacs-config))
             (-when-let (cond-config (wacs--get-cond-config-from-alist
                                      mode-config))
               (cl-return-from find-config cond-config))
             (setq mode-alias (get major-mode 'wacs-alias))
             (-when-let (cond-alias (wacs--get-cond-config-from-alist
                                     mode-alias))
               (cl-return-from find-config
                 (wacs--get-aliased-config cond-alias)))
             (-when-let (mode-default-config
                         (wacs--get-default-config-from-alist
                          mode-config))
               (cl-return-from find-config mode-default-config))
             (-when-let (mode-default-alias
                         (wacs--get-default-config-from-alist
                          mode-alias))
               (cl-return-from find-config
                 (wacs--get-aliased-config mode-default-alias)))
             (setq global-default-config
                   (get :default 'wacs-config))
             (-when-let (default-cond-config
                          (wacs--get-cond-config-from-alist
                           global-default-config))
               (cl-return-from find-config default-cond-config))
             (-when-let (default-config
                          (wacs--get-default-config-from-alist
                           global-default-config))
               (cl-return-from find-config default-config)))))
      (wacs--resolve-prefix config arg))))

(defun wacs--process-config (config)
  "Process CONFIG for inclusion in `wacs-config'."
  (let ((default-conf (-map 'wacs--list->dotted-pair
                            (wacs--alist-get :default config))))
    (unless default-conf
      (error
       "Please include a :default configuration for the wacspace"))
    (-map
     (lambda (entry)
       (cond ((eq (car entry) :default)
              (cons :default default-conf))
             ((memq (car entry) wacs--numeric-confs)
              (append (cons (car entry)
                            (-map 'wacs--list->dotted-pair (cdr entry)))
                      default-conf))
             (t (wacs--list->dotted-pair entry))))
     config)))

;;;###autoload
(defun wacs--push-config (mode aux-cond entry propname)
  "Given MODE and AUX-COND, push ENTRY with property name PROPNAME."
  (let ((aux-cond-key (or aux-cond :default))
        (mode-list (get mode propname)))
    (put mode propname
         (cons (cons aux-cond-key entry)
               (cl-delete-if (lambda (existing-pair)
                               (equal (car existing-pair) aux-cond-key))
                             mode-list)))
    t))

;;;###autoload
(defmacro defwacspace (condition &rest configuration)
  "Define a wacspace.

The CONDITION can either be a major mode (such as
`emacs-lisp-mode') or a (MAJOR-MODE AUXILIARY-CONDITION) pair.
The auxiliary condition can be a variable (such as a minor mode),
an inline lambda, or a (:fn FN) pair.  For full documentation of
CONFIGURATION options, see the README.

Some examples:

\(defwacspace python-mode
  ...)
-Default wacspace for `python-mode' buffers.

\(defwacspace  (ruby-mode rinari-minor-mode)
  ...)
-Activates in `ruby-mode' when `rinari-minor-mode' is turned on.

\(defwacspace (clojure-mode (lambda ()
                             (string-match \"test\" buffer-file-name)))
  ...)
-Activates in `clojure-mode' when \"test\" is in the buffer's file name.

\(defwacspace (java-mode (:fn is-enterprisy))
  ...)
-Activates in `java-mode' when function `is-enterprisy' evalutates to non-nil."

  (let ((mode (car (wacs--to-cons condition)))
        (aux-cond (cadr (wacs--to-cons condition)))
        (entry (wacs--process-config configuration)))
    `(wacs--push-config ',mode ',aux-cond ',entry 'wacs-config)))

;;;###autoload
(defmacro defwacsalias (condition target-condition)
  "Define a wacspace alias from CONDITION to TARGET-CONDITION.

When CONDITION is satisfied and `wacspace' is invoked, the
configuration for TARGET-CONDITION will be run.  For full details
of CONDITION and TARGET-CONDITION, see the docstring for
`defwacspace'"
  (let* ((mode (car (wacs--to-cons condition)))
         (aux-cond (cadr (wacs--to-cons condition)))
         (target-condition (wacs--to-cons target-condition))
         (entry (cons (car target-condition) (cadr target-condition))))
    `(wacs--push-config ',mode ',aux-cond ',entry 'wacs-alias)))

;;;###autoload
(defmacro defwacsaliases (conditions
                          target-condition)
  "Define multiple aliases for a mode.

CONDITIONS should be a list of conditions according to the rules
of `defwacspace'.  TARGET-CONDITION is the condition to alias to."
  (cons 'progn
        (append
         (-map (lambda (cond) `(defwacsalias ,cond
                            ,target-condition))
               conditions))))

;;;###autoload
(defmacro defwinconf (conf-name &rest body)
  "Define a wacspace window configuration named CONF-NAME.

BODY is the body of a function to be run when the configuration
is run (e.g. a sequence of window splitting commands).  The
function need not stop with the original window active."
  `(wacs--alist-put  ',conf-name
                     '(lambda () ,@body)
                     wacs--winconfs))

;;;###autoload
(defmacro wacs-set-frame-fn (frame fn)
  "Set the given FRAME parameter to FN.

FRAME and FN should be passed unquoted."
  `(wacs--alist-put ',frame ',fn wacs--frame-fns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation and Font Lock ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'wacs--with-property 'lisp-indent-function 1)
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<def\\(wacspace\\|winconf\\|wacsalias\\(es\\)?\\)\\>" .
    'font-lock-keyword-face)))

(provide 'wacs-configuration)

;;; wacs-configuration.el ends here
