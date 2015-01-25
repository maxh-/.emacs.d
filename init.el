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
(menu-bar-mode 0)
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

;;; Show matching parentheses
(show-paren-mode t)

;;; Use ido-mode
(ido-mode t)
(ido-everywhere t)
(setq ido-show-dot-for-dired t)

;;; Set frame size
(defun ss-80 ()
  (interactive)
  (set-frame-size (selected-frame) 80 65))
(defun ss-163 ()
  (interactive)
  (set-frame-size (selected-frame) 163 65))

;;;
;;; Plugins
;;;

;;; Smex (M-x enhancement, like Ido)
(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)
(setq smex-save-file "~/.emacs.d/plugin-data/smex/smex-items")

;;; Interactive bookmark jump
(require 'bookmark)
(defun ido-bookmark-jump (bname)
  "*Switch to bookmark interactively using `ido'."
  (interactive (list 
		(ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))

(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)

;;; Dired subtree viewing with 'i'
(define-key dired-mode-map (kbd "i") 'dired-subtree-toggle)

;;;
;;; Theme
;;;

;; Custom theme directory
(setq custom-theme-directory "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; Treat all themes as safe
(custom-set-variables
 '(custom-safe-themes t))

;; Theme
(load-theme 'tomorrow-night t)

;; Custom colors & fonts
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#1d1f21"
			 :foreground "#c5c8c6" :inverse-video nil
			 :box nil :strike-through nil :overline nil
			 :underline nil :slant normal :weight normal
			 :height 100 :width normal :foundry "bitstream"
			 :family "Meslo LG M DZ")))))

(lisp-extra-font-lock-global-mode t)
