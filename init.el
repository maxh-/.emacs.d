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

;;; Make executables work on OSX
(exec-path-from-shell-initialize)

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

;;;
;;; Custom keybindings etc
;;;

;;; Slime setup
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;; Smex (M-x enhancement, like Ido)
(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)
(setq smex-save-file "~/.emacs.d/plugin-data/smex/smex-items")

;;; Interactive bookmark jump (requires ido)
(require 'bookmark)
(defun ido-bookmark-jump (bname)
  (interactive (list 
		(ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))
(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)

;;; Dired subtree viewing with 'i'
(require 'dired)
(define-key dired-mode-map (kbd "i") 'dired-subtree-toggle)

;;; Toggle show hidden files with C-c h
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(define-key dired-mode-map (kbd "C-c h") 'dired-omit-mode)

;;; Request sudo password if file requires SU permissions
(defadvice ido-find-file (after find-file-sudo activate)
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;
;;; Theme
;;;

;;; Custom theme directory
(setq custom-theme-directory "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;;; Treat all themes as safe
(custom-set-variables
 '(custom-safe-themes t))

;;; Theme
(load-theme 'tomorrow-night t)

;;; Custom colors & fonts
(lisp-extra-font-lock-global-mode t)

;;; Font
(set-face-font 'default "-misc-fixed-medium-r-normal-*-14-*-*-*-*-*-iso10646-1")

;;;
;;; LaTeX
;;;

;;; Use xelatex
(custom-set-variables
 '(latex-run-command "xelatex")
 '(pdf-latex-command "xelatex")
 '(tex-run-command "xelatex"))
