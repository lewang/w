;;; w.el --- Thin workspace manager on top of tab-bar-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Le Wang

;; Author: Le Wang
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, frames
;; URL: https://github.com/lewang/w

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A workspace is just a name + project root + reset function.  The reset
;; function sets up windows/buffers in a fresh tab.  No buffer tracking,
;; window state persistence, autosave, or hidden hooks.

;;; Code:

(require 'tab-bar)

;;; Variables

(defvar w-workspaces nil
  "List of workspace plists.
Each plist has keys :name, :project-root, and :reset-function.")

(defvar w-after-reset-hook nil
  "Hook run after a workspace reset-function is called in a new tab.
Each function receives the workspace plist as its argument.")

(defvar w-after-switch-hook nil
  "Hook run after switching to an existing workspace tab.
Each function receives the workspace plist as its argument.")

(defvar w-name-prefix "w: "
  "Prefix for tab names created by w.")

;;; Internal functions

(defun w--find-workspace (name)
  "Find workspace plist in `w-workspaces' by NAME."
  (seq-find (lambda (ws) (string= (plist-get ws :name) name))
            w-workspaces))

(defun w--find-tab (name)
  "Scan tabs for one with `w-workspace' = NAME.  Return tab or nil."
  (seq-find (lambda (tab)
              (let ((ws-name (alist-get 'w-workspace (cdr tab))))
                (and ws-name (string= ws-name name))))
            (funcall tab-bar-tabs-function)))

(defun w--set-tab-workspace (name)
  "Store workspace NAME on the current tab."
  (setf (alist-get 'w-workspace (cdr (tab-bar--current-tab-find))) name))

(defun w--tab-workspace-name (&optional tab)
  "Get workspace name from TAB (current tab if nil)."
  (alist-get 'w-workspace (cdr (or tab (tab-bar--current-tab-find)))))

(defun w--read-workspace (prompt &optional default)
  "Completing-read over workspace names with PROMPT.
Optional DEFAULT is used as the default value."
  (let ((names (mapcar (lambda (ws) (plist-get ws :name)) w-workspaces)))
    (completing-read prompt names nil t nil nil default)))

(defun w--read-reset-function (prompt &optional default)
  "Completing-read for a function with PROMPT.
Optional DEFAULT is a symbol used as the default value."
  (intern (completing-read prompt obarray #'functionp t nil nil
                           (when default (symbol-name default)))))

;;; Commands

;;;###autoload
(defun w-go (name)
  "Switch to workspace NAME.
If a tab exists for this workspace, switch to it and run
`w-after-switch-hook'.  Otherwise create a new tab, set
`default-directory' to the workspace's project-root, call the
reset-function with project-root, rename the tab, and run
`w-after-reset-hook'."
  (interactive (list (w--read-workspace "Workspace: ")))
  (let ((ws (w--find-workspace name)))
    (unless ws
      (user-error "No workspace named %s" name))
    (let ((tab (w--find-tab name)))
      (if tab
          (progn
            (tab-bar-switch-to-tab (alist-get 'name (cdr tab)))
            (run-hook-with-args 'w-after-switch-hook ws))
        (let ((project-root (plist-get ws :project-root))
              (reset-fn (plist-get ws :reset-function)))
          (tab-bar-new-tab)
          (tab-bar-rename-tab (concat w-name-prefix name))
          (w--set-tab-workspace name)
          (let ((default-directory project-root))
            (funcall reset-fn project-root))
          (run-hook-with-args 'w-after-reset-hook ws))))))

;;;###autoload
(defun w-new (&rest args)
  "Add a workspace.
Interactively, prompt for name, project-root, and reset-function.
Programmatically, accept a plist (:name ... :project-root ... :reset-function ...).
Does NOT create a tab; use `w-go' for that."
  (interactive
   (list :name (read-string "Workspace name: ")
         :project-root (read-directory-name "Project root: ")
         :reset-function (w--read-reset-function "Reset function: ")))
  (let* ((name (plist-get args :name))
         (project-root (plist-get args :project-root))
         (reset-fn (plist-get args :reset-function))
         (ws (list :name name
                   :project-root project-root
                   :reset-function reset-fn)))
    (when (w--find-workspace name)
      (user-error "Workspace %s already exists" name))
    (push ws w-workspaces)
    ws))

;;;###autoload
(defun w-delete (name)
  "Remove workspace NAME from `w-workspaces'.
If it has an open tab, close that tab too."
  (interactive (list (w--read-workspace "Delete workspace: ")))
  (let ((ws (w--find-workspace name)))
    (unless ws
      (user-error "No workspace named %s" name))
    (let ((tab (w--find-tab name)))
      (when tab
        (tab-bar-close-tab-by-name (alist-get 'name (cdr tab)))))
    (setq w-workspaces (seq-remove (lambda (w) (string= (plist-get w :name) name))
                                   w-workspaces))))

;;;###autoload
(defun w-edit (name)
  "Edit workspace NAME's fields.
Interactively, prompt for each field prepopulated with current values.
Defaults to current workspace if in one."
  (interactive
   (list (let ((current (w--tab-workspace-name)))
           (if current
               (w--read-workspace (format "Edit workspace (default %s): " current) current)
             (w--read-workspace "Edit workspace: ")))))
  (let ((ws (w--find-workspace name)))
    (unless ws
      (user-error "No workspace named %s" name))
    (let* ((old-name (plist-get ws :name))
           (new-name (read-string "Name: " old-name))
           (new-root (read-directory-name "Project root: " (plist-get ws :project-root)))
           (new-fn (w--read-reset-function
                    (format "Reset function (default %s): "
                            (plist-get ws :reset-function))
                    (plist-get ws :reset-function))))
      (plist-put ws :name new-name)
      (plist-put ws :project-root new-root)
      (plist-put ws :reset-function new-fn)
      ;; If the name changed, update the tab too
      (unless (string= old-name new-name)
        (let ((tab (w--find-tab old-name)))
          (when tab
            (let ((idx (1+ (seq-position (funcall tab-bar-tabs-function) tab #'eq))))
              (tab-bar-rename-tab (concat w-name-prefix new-name) idx)
              ;; Update the workspace key on the tab
              (setf (alist-get 'w-workspace (cdr tab)) new-name)))))
      ws)))

(defun w-current ()
  "Return current workspace plist or nil."
  (when-let ((name (w--tab-workspace-name)))
    (w--find-workspace name)))

(provide 'w)
;;; w.el ends here
