;;; init.el --- emacs init file.

;;;
;;; Online package repository.
;;;

;; Enable MELPA package repository.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Autoinstall `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Autoload custom folder ~/.emacs.d/elisp.
(defvar custom-elisp-folder (concat user-emacs-directory "elisp"))
(when (file-exists-p custom-elisp-folder)
  (add-to-list 'load-path custom-elisp-folder))

;;;
;;; General editor config.
;;;

;; Use UTF-8 encoding
(setq default-buffer-file-coding-system 'utf-8-unix)

;; Allow custom themes
(setq custom-safe-themes t)

;; Use visual line wrapping
(visual-line-mode t)

;; Put auto generated elisp in custom folder
(setq custom-file (concat user-emacs-directory "elisp""~/.emacs-custom.el"))
(load custom-file 'noerror)

;; Remove bling.
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(visual-line-mode t)
(setq initial-scratch-message "")
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Get aliases from bash
(setq shell-file-name "bash")
(setq shell-command-sw/itch "-ic")

;; Prompt less.
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

;; Don't litter working tree with temp files.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

;; Default indentation settings.
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)

;; Make backspace work like expected when searching.
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; Display some info in modeline.
(line-number-mode t)
(column-number-mode t)

;; Highlight matching parenthesis or bracket.
(show-paren-mode t)

;;Show line numbers in text buffers.
(setq linum-format "%4d ")
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'conf-windows-mode-hook 'linum-mode)
(add-hook 'conf-unix-mode-hook 'linum-mode)

;; Style line numbers.
(fringe-mode '(7 . 4))
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

;; Use ido-mode for finding files with `C-x C-f'
(ido-mode t)
(ido-everywhere t)
(setq ido-show-dot-for-dired t)
(setq ido-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)

;; Enable colors in `eshell-mode'.
(require 'ansi-color)
(add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

;; Open help windows in the active window.
(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))

;; Use python3 interpeter
(setq python-shell-interpreter "/usr/bin/python3")

;; Resize mode

;;;
;;; Utility functions.
;;;

(defun open-terminal-below ()
  "Split the window vertically and open a terminal below it."
  (interactive)
  (progn (split-window-below)
         (other-window 1)
         (eshell)))

;;;
;;; Global keybindings.
;;;

;; Open an eshell under current window.
(global-set-key (kbd "C-c t") 'open-terminal-below)

;; Cycle through windows with C-x C-o.
(bind-key* (kbd "C-x C-o") (lambda () (interactive) (other-window 1)))

;; Kill buffer+window when killing a buffer.
(bind-key* (kbd "C-x k") 'kill-buffer-and-window)

;; Vim-style window movement with C-x C-h/j/k/l
(bind-key* "C-x C-l" 'windmove-right)
(bind-key* "C-x C-j" 'windmove-down)
(bind-key* "C-x C-k" 'windmove-up)
(bind-key* "C-x C-h" 'windmove-left)

;; Open dired in the current location using C-x d
(bind-key* "C-x d" 'open-dired-here)
(defun open-dired-here ()
  (interactive)
  (dired-at-point "."))

;; Run single shell command.
(bind-key* "C-c s" 'shell-command)

;; Run last shell command again.

(bind-key* "C-c S" (lambda () (interactive)
                     (shell-command (cadr (assoc 'shell-command command-history)))))


;;;
;;; Third-party packages.
;;;

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-oceanicnext))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package dired
  :defer t
  :bind (:map dired-mode-map ("C-u" . dired-up-directory))
  :init)

(use-package dired-hide-dotfiles
  :ensure t
  :bind (:map dired-mode-map ("C-c ." . dired-hide-dotfiles-mode))
  :init
  (add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode))

(use-package immortal-scratch
  :ensure t
  :config
  (immortal-scratch-mode t))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'")
  :config
  (setq js2-basic-offset 4)
  (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
  (define-key js2-mode-map "@" 'js-doc-insert-tag)
  (setq js2-concat-multiline-strings t))

(use-package js-doc
  :ensure t)

(use-package python-mode
  :ensure t
  :defer t)

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package magit
  :ensure t
  :defer t)

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package smex
  :ensure t
  :bind* ("M-x" . smex)
  :init
  (global-set-key (kbd "M-x") 'smex)
  (defvar smex-save-file
    (concat user-emacs-directory "plugin-data/smex/smex-items")))

(use-package sudo-edit :ensure t :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2))

(use-package markdown-mode
  :ensure t
  :mode "\\.MD\\'")

(use-package org
  :ensure t
  :mode "\\.org\\'"
  :config
  (define-key
    org-mode-map
    [remap org-meta-return] 'org-insert-heading-respect-content)
  (setq org-cycle-separator-lines 1))

(use-package edit-server
  :ensure t
  :config
  ;; Set up Emacs server
  (setq edit-server-url "127.0.0.1")
  (setq  server-port 9292)
  (require 'edit-server)
  (edit-server-stop)
  (edit-server-start))

(use-package smooth-scroll
  :ensure t
  :config
  (require 'smooth-scroll)
  (smooth-scroll-mode)
  (setq smooth-scroll/vscroll-step-size 2))

(use-package minibuffer-line
  :ensure t
  :config
  (minibuffer-line-mode t))

(use-package paredit
  :ensure t
  :mode ("\\.el\\'")
  :config
  (emacs-lisp-mode)
  (paredit-mode))

;;;
;;; Fonts.
;;;

(cond
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-frame-font "DejaVu Sans Mono-14"))
 ((find-font (font-spec :name "inconsolata"))
  (set-frame-font "inconsolata-12"))
 ((find-font (font-spec :name "courier"))
  (set-frame-font "courier-11")))

;;; init.el ends here
