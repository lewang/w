;;; w.el --- Thin workspace manager on top of tab-bar-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Le Wang

;; Author: Le Wang
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, workspaces
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

;; A workspace is a name + project root + reset function.  The reset
;; function receives the project root and sets up windows/buffers in a
;; fresh tab.  No buffer tracking, window-state persistence, autosave,
;; or hidden hooks.
;;
;; Usage:
;;
;;   (w-new :name "myproject"
;;          :project-root "~/src/myproject/"
;;          :reset-function #'find-file)
;;
;;   (w-go "myproject")    ; creates tab, calls reset function
;;   (w-go "myproject")    ; switches to existing tab
;;
;; Persist across sessions with savehist:
;;
;;   (add-to-list 'savehist-additional-variables 'w-workspaces)

;;; Code:

(require 'tab-bar)

(defgroup w nil
  "Thin workspace manager on top of tab-bar-mode."
  :group 'convenience
  :prefix "w-")

;;; Variables

(defvar w-workspaces nil
  "List of workspace plists.
Each plist has keys :name, :project-root, and :reset-function.")

(defcustom w-default-reset-function #'find-file
  "Default reset function for new workspaces.
Called with the project root directory as its sole argument."
  :type 'function
  :group 'w)

(defcustom w-after-reset-hook nil
  "Hook run after a workspace reset-function is called in a new tab.
Each function receives the workspace plist as its argument."
  :type 'hook
  :group 'w)

(defcustom w-after-switch-hook nil
  "Hook run after switching to an existing workspace tab.
Each function receives the workspace plist as its argument."
  :type 'hook
  :group 'w)

(defcustom w-name-prefix "w: "
  "Prefix for tab names created by w."
  :type 'string
  :group 'w)

;;; Internal functions

(defun w--find-workspace (name)
  "Find workspace plist in `w-workspaces' by NAME."
  (seq-find (lambda (ws) (string= (plist-get ws :name) name))
            w-workspaces))

(defun w--find-tab (name)
  "Search all frames for a tab tagged with workspace NAME.
Return (TAB . FRAME) or nil."
  (catch 'found
    (dolist (frame (frame-list))
      (dolist (tab (funcall tab-bar-tabs-function frame))
        (when (equal name (alist-get 'w-workspace (cdr tab)))
          (throw 'found (cons tab frame)))))
    nil))

;; NOTE: `tab-bar--current-tab-find' is internal to tab-bar.el but
;; is the only way to get a mutable reference to the current tab's
;; alist.  No public API exists for storing custom per-tab data.

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
  "Switch to workspace NAME, searching all frames for its tab.
If a tab exists for this workspace on any frame, select that
frame and switch to the tab, then run `w-after-switch-hook'.
Otherwise create a new tab on the current frame, set
`default-directory' to the workspace's project-root, call the
reset-function with project-root, rename the tab, and run
`w-after-reset-hook'."
  (interactive (list (w--read-workspace "Workspace: ")))
  (let ((ws (w--find-workspace name)))
    (unless ws
      (user-error "No workspace named %s" name))
    (let ((found (w--find-tab name)))
      (if found
          (let ((tab (car found))
                (frame (cdr found)))
            (select-frame-set-input-focus frame)
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
Programmatically, accept a plist with keys :name, :project-root,
and :reset-function.  Does NOT create a tab; use `w-go' for that."
  (interactive
   (let* ((root (read-directory-name "Project root: " default-directory))
          (base (file-name-nondirectory (directory-file-name root))))
     (list :name (read-string "Workspace name: " base)
           :project-root root
           :reset-function (w--read-reset-function
                            (format "Reset function (default %s): "
                                    w-default-reset-function)
                            w-default-reset-function))))
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
    (when-let* ((found (w--find-tab name)))
      (let ((tab (car found))
            (frame (cdr found)))
        (tab-bar-close-tab-by-name (alist-get 'name (cdr tab)) frame)))
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
        (when-let* ((found (w--find-tab old-name)))
          (let* ((tab (car found))
                 (frame (cdr found))
                 (tabs (funcall tab-bar-tabs-function frame))
                 (idx (1+ (seq-position tabs tab #'eq))))
            (tab-bar-rename-tab (concat w-name-prefix new-name) idx frame)
            (setf (alist-get 'w-workspace (cdr tab)) new-name))))
      ws)))

(defun w-current ()
  "Return current workspace plist or nil."
  (when-let* ((name (w--tab-workspace-name)))
    (w--find-workspace name)))

;;; Visit

(defun w--target-dir (target)
  "Return the directory associated with TARGET.
TARGET is a buffer name or file path."
  (if-let* ((buf (get-buffer target)))
      (buffer-local-value 'default-directory buf)
    (when (file-exists-p target)
      (if (file-directory-p target)
          (file-name-as-directory target)
        (file-name-directory (expand-file-name target))))))

(defun w--find-best-workspace (target-dir)
  "Return the workspace whose :project-root best matches TARGET-DIR.
Best match is the workspace with the longest project-root that is
a prefix of TARGET-DIR."
  (let ((target-dir (expand-file-name (file-name-as-directory target-dir)))
        best best-len)
    (dolist (ws w-workspaces)
      (when-let* ((dir (plist-get ws :project-root))
                  (dir (expand-file-name (file-name-as-directory dir))))
        (when (and (string-prefix-p dir target-dir)
                   (or (null best-len) (> (length dir) best-len)))
          (setq best ws
                best-len (length dir)))))
    best))

;;;###autoload
(defun w-visit (target)
  "Visit TARGET in its matching workspace.
TARGET is a buffer name or file path.  Find a workspace whose
project-root contains TARGET, or create one from `project-current'.
Then switch to that workspace (crossing frames if needed) and
display TARGET there."
  (interactive
   (list (completing-read "Buffer or file: "
                          (completion-table-merge
                           #'internal-complete-buffer
                           #'read-file-name-internal))))
  (let* ((target-dir (w--target-dir target))
         (ws (when target-dir (w--find-best-workspace target-dir))))
    (unless ws
      (let* ((proj (or (and target-dir
                            (let ((default-directory target-dir))
                              (project-current)))
                       (user-error "Cannot determine project for %s" target)))
             (root (expand-file-name
                    (file-name-as-directory (project-root proj)))))
        (setq ws (or (seq-find (lambda (w)
                                 (string= root
                                          (expand-file-name
                                           (file-name-as-directory
                                            (plist-get w :project-root)))))
                               w-workspaces)
                     (w-new :name (file-name-nondirectory
                                   (directory-file-name root))
                            :project-root root
                            :reset-function w-default-reset-function)))))
    (w-go (plist-get ws :name))
    (pop-to-buffer (if (get-buffer target)
                       target
                     (find-file-noselect target)))))

(defvar embark-buffer-map)
(defvar embark-file-map)

;;;###autoload
(with-eval-after-load 'embark
  (define-key embark-buffer-map (kbd "v") #'w-visit)
  (define-key embark-file-map   (kbd "v") #'w-visit))

(provide 'w)
;;; w.el ends here
