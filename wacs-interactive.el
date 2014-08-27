;;; wacs-interactive.el --- Interactive commands for wacspace -*- lexical-binding: t -*-

;; Copyright © 2013-2014 Emanuel Evans

;; Author: Emanuel Evans <emanuel.evans@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Interactive commands for wacspace.el.

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

(require 'wacs-util)
(require 'wacs-configuration)

;; Declare variables defined in wacs-configuration to keep the
;; compiler happy

(defvar wacs-regexp-buffer-switching)
(defvar wacs-project-base-file)
(defvar wacs-save-frame)

(defun wacs--run-winconf (conf-name)
  "Run winconf with name CONF-NAME."
  (delete-other-windows)
  (-if-let (winconf (wacs--alist-get conf-name wacs--winconfs))
      (let ((main-window (selected-window)))
        (funcall winconf)
        (select-window main-window)
        main-window)
    (error "No winconf with name: %s" conf-name)))

(defun wacs--set-frame (frame)
  "Set the frame using the function set for FRAME."
  (-if-let (frame-fn (wacs--alist-get frame wacs--frame-fns))
      (funcall frame-fn)
    (message "No frame fn specified for frame alignment %s" frame)))

(cl-defmacro wacs--with-property ((prop) &body body)
  "Helper macro for using properties within configurations."
  (let ((prop-keyword (intern (concat ":" (symbol-name prop)))))
    `(let ((,prop (wacs--alist-get ,prop-keyword config)))
       (when ,prop
         ,@body))))

(defun wacs--switch-to-buffer (buffer-string)
  "Switch to buffer with name BUFFER-STRING.

If `wacs-regexp-buffer-switching' is set to t, BUFFER-STRING is
interpreted as an unescaped regexp."
  (-if-let (buffer
            (car (--filter (string= buffer-string
                                    (buffer-name it))
                           (buffer-list))))
      (switch-to-buffer buffer)
    (if wacs-regexp-buffer-switching
        (-if-let (buffer
                  (car
                   (--filter
                    (string-match-p (regexp-quote buffer-string)
                                    (buffer-name it))
                    (buffer-list))))
            (switch-to-buffer buffer)
          (switch-to-buffer buffer-string))
      (switch-to-buffer buffer-string))))

(defmacro wacs--update-local-vars ()
  "Update local vars in the current buffer.

Variables from `wacs--persistent-local-vars' will be updated."
  (cons 'progn
        (-map (lambda (var)
                `(setq-local ,var ,var))
              wacs--persistent-local-vars)))

(defun wacs--set-up-windows (config main-window)
  "Set up the windows according to the CONFIG.

MAIN-WINDOW is the window from which `wacspace' was called."
  (-each (-take (length (window-list))
                '(:main :aux1 :aux2 :aux3 :aux4 :aux5))
    (lambda (win-key)
      (-when-let (buffer-conf (wacs--alist-get win-key config))
        (select-window main-window)
        (other-window (string-to-number
                       (substring (symbol-name win-key) -1)))
        (cond ((or (eq buffer-conf :main)
                  (and (consp buffer-conf) (eq (cdr buffer-conf) :main)))
               (switch-to-buffer wacs-main-buffer))
              ((stringp buffer-conf)
               (wacs--switch-to-buffer buffer-conf))
              ((symbolp buffer-conf)
               (funcall buffer-conf))
              ;; Backwards-compatibility for (:buffer "foo") syntax
              ((and (consp buffer-conf) (eq (car buffer-conf) :buffer))
               (wacs--switch-to-buffer (cdr buffer-conf)))
              ;; Backwards-compatibility for (:cmd bar) syntax
              ((and (consp buffer-conf) (eq (car buffer-conf) :cmd))
               (funcall (cdr buffer-conf)))
              (t (error "Invalid wacspace buffer configuration."))))
      (wacs--update-local-vars)))
  (wacs--switch-to-window-with-buffer wacs-main-buffer))

(defun wacs--set-up-workspace (config)
  "Set up the workspace according to CONFIG."
  (let ((wacs-project-base-file
         (or (wacs--alist-get :base-file config)
            wacs-project-base-file
            (file-name-nondirectory (buffer-file-name))))
        (wacs--project-name-fn (wacs--alist-get :project-name-fn
                                                config)))
    (wacs--with-property (before)
      (save-window-excursion
        (funcall before)))
    (wacs--with-property (frame)
      (wacs--set-frame frame))
    (let ((main-window
           (wacs--with-property (winconf)
             (wacs--run-winconf winconf))))
      (wacs--set-up-windows config main-window))
    (wacs--with-property (run)
      (save-window-excursion
        (funcall run)))
    (wacs--with-property (after)
      (save-window-excursion
        (funcall after)))
    (wacs--with-property (after-switch)
      (puthash (wacs-project-name)
               after-switch
               wacs--after-switch-fns)
      (save-window-excursion
        (funcall after-switch)))
    (message "wacspace configured")))

(defun wacs--update-open-projects (buffer arg)
  "Update `wacs--open-projects' with BUFFER and ARG."
  (let ((project-name (wacs-project-name)))
    (wacs--alist-put project-name
                     (cons buffer arg)
                     wacs--open-projects)))

;;;###autoload
(defun wacspace (&optional arg)
  "Set up your Emacs workspace.

If there is a saved configuration with numeric prefix ARG,
restore that.  Otherwise, set up your workspace based on your
wacspace configuration.  If called with universal prefix
arg (\\[universal-argument]), force reconfiguration even if there
is a saved workspace."
  (interactive "P")
  (when (wacs--u-prefix? arg)
    (wacs-clear-saved (current-buffer))
    (setq arg nil))
  (unless (wacspace-restore arg)
    (-if-let* ((wacs-main-buffer (current-buffer))
               (config (wacs--get-config arg)))
        (progn (wacs--set-up-workspace config)
               (wacspace-save arg))
      (error
       "No wacspace configuration available for the current mode"))))

;;;###autoload
(defun wacspace-save (&optional arg)
  "Save the current window configuration with prefix ARG.

When wacspace is invoked in the future in any of the current
buffers with given prefix key, the current workspace will be
restored."
  (interactive "P")
  (let* ((config-symbol-alist
          (gethash (current-buffer)
                   wacs--saved-workspaces))
         (config (if wacs-save-frame
                     (current-frame-configuration)
                   (current-window-configuration)))
         (current-buffers (-map 'window-buffer (window-list)))
         (new-config-alist
          (wacs--alist-put (or arg :default)
                           (cons config current-buffers)
                           config-symbol-alist)))
    (--each (window-list)
      (puthash (window-buffer it)
               new-config-alist
               wacs--saved-workspaces))
    (wacs--update-open-projects (current-buffer) arg)
    (message "wacspace saved")))

;;;###autoload
(defun wacspace-restore (&optional arg)
  "Restore a window configuration saved with prefix key ARG.

Usually, you should call `wacspace' directly instead of this
function unless you want to skip the possibility of
configuration."
  (let ((buffer (current-buffer))
        (buffer-points
         (-map (lambda (b) (cons b (wacs--buffer-point b))) (buffer-list))))
    (ignore-errors
      (-when-let (config (cadr
                          (assoc (or arg :default)
                                 (gethash buffer
                                          wacs--saved-workspaces))))
        (if wacs-save-frame
            (set-frame-configuration config)
          (set-window-configuration config))
        (--each (wacs--interesting-buffers)
          (wacs--set-buffer-point it (wacs--alist-get it buffer-points)))
        (wacs--switch-to-window-with-buffer buffer)
        (wacs--update-open-projects (current-buffer) arg)
        (message "wacspace restored")
        t))))

(defun wacspace-switch-project ()
  "Quickly switch between open projects."
  (interactive)
  (if (null wacs--open-projects)
      (message "No open projects")
    (let* ((project-names (-map 'car wacs--open-projects))
           (project (completing-read "Project: " project-names
                                     nil t nil nil
                                     (if (= (length project-names) 1)
                                         (car project-names)
                                       (cadr project-names))))
           (config (wacs--alist-get project wacs--open-projects))
           (buffer (car config))
           (last-prefix (cdr config)))
      (switch-to-buffer buffer)
      (wacspace last-prefix)
      (-when-let (after-switch (gethash project
                                        wacs--after-switch-fns))
        (funcall after-switch)))))

(defun wacs-clear-saved (&optional buffer)
  "Clear saved workspaces associated with BUFFER.

BUFFER can be a string or a buffer object.  If called
interactively, will clear saved workspaces associated with the
current buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (-each (gethash buffer wacs--saved-workspaces)
      (lambda (entry)
        (-each (cddr entry)
          (lambda (buffer)
            (remhash buffer wacs--saved-workspaces)))))))

(defun wacs-clear-all-saved ()
  "Clear all saved workspaces from this session."
  (interactive)
  (maphash (lambda (key _) (wacs-clear-saved key))
           wacs--saved-workspaces))

(defun wacs--kill-buffer-hook ()
  "Hook to clear saved associated workspaces when a buffer is killed."
  (wacs-clear-saved (current-buffer)))

(add-hook 'kill-buffer-hook 'wacs--kill-buffer-hook)

(provide 'wacs-interactive)

;;; wacs-interactive.el ends here
