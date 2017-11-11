;;; init.el --- emacs init file.
;; Use MELPA package repository.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Autoinstall `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Add ~/.emacs.d/elisp to load path.
(defvar custom-elisp-folder (concat user-emacs-directory "elisp"))
(when (file-exists-p custom-elisp-folder)
  (add-to-list 'load-path custom-elisp-folder))

;;window [class="^.*"] border pixel 1

;;;
;;; General config.
;;;

;; Remove bling.
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

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

;; Use ido-mode for finding files.
(ido-mode t)
(ido-everywhere t)
(setq ido-show-dot-for-dired t)
(setq ido-flex-matching t)

;; Enable colors in `eshell-mode'.
(require 'ansi-color)
(add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

;; Open help windows in the active window.
(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))

;; Use python3 interpeter
(setq python-shell-interpreter "/usr/bin/python3")

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

;;;
;;; Packages.
;;;

(use-package base16-theme
  :ensure t
  :config
  (setq custom-safe-themes t)
  (load-theme 'base16-ocean))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package dired
  :defer t
  :bind (:map dired-mode-map ("C-u" . dired-up-directory))
  :init
  ;; Show folders first in dired mode
  (setq dired-listing-switches "-aBhl  --group-directories-first"))


(use-package dired-hide-dotfiles
  :ensure t
  :defer t
  :bind (:map dired-mode-map ("C-c ." . dired-hide-dotfiles-mode))
  :init
  (add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode))

(use-package immortal-scratch
  :ensure t
  :config
  (immortal-scratch-mode t))

(use-package js-doc
  :ensure t
  :mode "\\.js\\'")

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'")
  :init
  (setq js2-basic-offset 2))

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

;;;
;;; Fonts.
;;;

(cond
 ((find-font (font-spec :name "DejaVu Sans"))
  (set-frame-font "DejaVu Sans Mono-11"))
 ((find-font (font-spec :name "inconsolata"))
  (set-frame-font "inconsolata-12"))
 ((find-font (font-spec :name "courier"))
  (set-frame-font "courier-11")))

;;; init.el ends here
