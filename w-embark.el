;;; w-embark.el --- Embark integration for w workspaces  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Le Wang

;; Author: Le Wang
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (w "0.1.0") (embark "1.0"))

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

;; Visit buffers and files in their matching w workspace via Embark.
;; Binds "v" in `embark-buffer-map' and `embark-file-map' to
;; `w-embark-visit'.

;;; Code:

(require 'w)
(require 'embark)

(defun w-embark--target-dir (target)
  "Return the directory associated with TARGET.
TARGET is a buffer name or file path."
  (if-let* ((buf (get-buffer target)))
      (buffer-local-value 'default-directory buf)
    (when (file-exists-p target)
      (if (file-directory-p target)
          (file-name-as-directory target)
        (file-name-directory (expand-file-name target))))))

(defun w-embark--find-best (target-dir)
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
(defun w-embark-visit (target)
  "Visit TARGET in its matching w workspace.
TARGET is a buffer name or file path.  Find a workspace whose
project-root contains TARGET, or create one from `project-current'.
Then switch to that workspace (crossing frames if needed) and
display TARGET there."
  (interactive "sBuffer or file: ")
  (let* ((target-dir (w-embark--target-dir target))
         (ws (when target-dir (w-embark--find-best target-dir))))
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

;;;###autoload
(with-eval-after-load 'embark
  (define-key embark-buffer-map (kbd "v") #'w-embark-visit)
  (define-key embark-file-map   (kbd "v") #'w-embark-visit))

(provide 'w-embark)
;;; w-embark.el ends here
