;;;
;;; General settings
;;;

;;; Enable the MELPA user repository.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;; Remove window decorations and general bling.
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;;; Make executables work on OSX from eshell.
(exec-path-from-shell-initialize)

;;; Put backup files in tmp directory so as to not clutter working tree.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

;;; Make emacs prompt less for file actions.
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
;; Don't ask to kill buffers with active processes.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

;;; Show some useful info in modeline.
(display-time-mode t)
(line-number-mode t)
(column-number-mode t)

;;; Show matching parentheses.
(show-paren-mode t)

;;; Use ido-mode everywhere.
(ido-mode t)
(ido-everywhere t)
(setq ido-show-dot-for-dired t)

;;;
;;; Custom keybindings and plugins.
;;;

;;; Slime setup (LISP interactive shell).
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;; Smex (M-x enhancement, like Ido).
(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)
(setq smex-save-file "~/.emacs.d/plugin-data/smex/smex-items")

;;; Toggle show hidden files with C-c h.
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(define-key dired-mode-map (kbd "C-c h") 'dired-omit-mode)

;;; Request sudo password if file requires SU permissions.
(defadvice ido-find-file (after find-file-sudo activate)
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;
;;; Indentation settings for different modes.
;;;

;;; Web-mode indentation settings.
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;;; Javascript-mode indentation settings.
(setq js-indent-level 2)

;;;
;;; Themes and fonts.
;;;

;;; Custom theme directory
(setq custom-theme-directory "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;;; Treat all themes as safe
(custom-set-variables
 '(custom-safe-themes t)
 '(magit-item-highlight-face (quote bold)))

;;; Theme
(load-theme 'tomorrow-night t)

;;; Custom colors & fonts
(lisp-extra-font-lock-global-mode t)
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#1d1f21" :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
