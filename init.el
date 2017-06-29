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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Editor config.
;;;

;; Remove bling.
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Prompt less.
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

;; Don't litter working tree with temp files.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

;; Scroll single lines.
(setq scroll-step 1)

;; Make backspace work like expected when searching.
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; Store autogenerated code in separate file.
(setq custom-file (concat user-emacs-directory "customize.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Cycle through windows with <C-return>.
(bind-key* "<C-return>" 'other-window)

;; Use `ibuffer' for listing buffers.
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;;; Display some info in modeline.
(display-time-mode t)
(line-number-mode t)
(column-number-mode t)

;;; Highlight matching parenthesis or bracket.
(show-paren-mode t)

;;; Use ido-mode for finding files.
(ido-mode t)
(ido-everywhere t)
(setq ido-show-dot-for-dired t)
(setq ido-flex-matching t)

;; Show folders first in `dired-mode'.
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; Enable colors in `eshell-mode'.
(require 'ansi-color)
(add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

;; Open help windows in the active window.
(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window))

;; Font.
(cond
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-frame-font "DejaVu Sans Mono-12"))
 ((find-font (font-spec :name "inconsolata"))
  (set-frame-font "inconsolata-12"))
 ((find-font (font-spec :name "Lucida Console"))
  (set-frame-font "Lucida Console-12"))
 ((find-font (font-spec :name "courier"))
  (set-frame-font "courier-12")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global keybindings.
;;;

;; Open an eshell in a new window below the current one with C-c t.
(defun open-terminal-below ()
  "Split the window vertically and open a terminal below it."
  (interactive)
  (progn (split-window-below)
         (other-window 1)
         (eshell)))
(global-set-key (kbd "C-c t") 'open-terminal-below)

;; Toggle fullscreen with C-c f.
(global-set-key (kbd "C-c f") 'toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Packages.
;;;

(use-package base16-theme
  :ensure t
  :config
  (setq custom-safe-themes t)
  (load-theme 'base16-tomorrow-night))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package org :defer t :ensure t)

(use-package flycheck :ensure t :defer t)

(use-package dired-hide-dotfiles
  :ensure t
  :defer t
  :bind (:map dired-mode-map ("C-c ." . dired-hide-dotfiles-mode))
  :init
  (require 'dired)
  (add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode))

(use-package immortal-scratch
  :ensure t
  :config
  (immortal-scratch-mode t))

(use-package js-doc :ensure t :mode "\\.js\\'")

(use-package js3-mode
  :ensure t
  :mode ("\\.js\\'" "\\.json\\'")
  :config
  (setq js3-indent-level 2))

(use-package nodejs-repl :ensure t :defer t)

(use-package eslint-fix
  :if (executable-find "eslint")
  :ensure t
  :defer t)

(use-package lua-mode :ensure t :mode "\\.lua\\'")

(use-package magit :ensure t :defer t)

(use-package php-mode :ensure t :mode "\\.php\\'")

(use-package sass-mode :ensure t :mode "\\.scss\\'")

(use-package slime
  :if (executable-find "sbcl")
  :ensure t
  :defer t
  :config
  (defvar inferior-lisp-program (executable-find "sbcl"))
  (setq slime-contribs '(slime-fancy)))

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

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode t)
  (setq smooth-scroll/vscroll-step-size 1))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

;;; init.el ends here
