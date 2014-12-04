;;;
;;; General
;;;

;;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;; Remove bling
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;;; Put backup files in tmp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

;;; Don't prompt so much
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
;; Don't ask to kill buffers with active processes
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;;; Show some useful info in modeline
(display-time-mode t)
(line-number-mode t)
(column-number-mode t)

;;; Use ido-mode
(ido-mode t)
(ido-everywhere t)

;;; 80 width master race
(setq-default fill-column 80)
(longlines-mode t)

;;;
;;; Plugins
;;;

;;; Smex (M-x enhancement)
(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)
(setq smex-save-file "~/.emacs.d/plugin-data/smex/smex-items")

;;; Ido
(defun ido-bookmark-jump (bname)
  "*Switch to bookmark interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))

(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)

;;;
;;; Theme
;;;

;; Theme directory
(setq custom-theme-directory "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; Treat all themes as safe
(custom-set-variables
 '(custom-safe-themes t))

;; Theme
(load-theme 'tomorrow-night t)

;; Custom colors
(custom-set-faces
 '(magit-item-highlight ((t (:background "gray36")))))
