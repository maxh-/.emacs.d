;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General editor settings.
;;;

;; Enable the MELPA repository on emacs versions 24 or newer.
;; View packages with `M-x list-packages`.

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Autoload all installed packages.

(package-initialize)

;; Remove window decorations and general bling.

(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Import the user's `$PATH` variable into eshell.

(exec-path-from-shell-initialize)

;; Put temporary files in tmp directory so as to not clutter working directory.

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

;; Make emacs prompt less for file actions.

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

;; Don't ask to kill buffers with active processes. (Annoying.)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Show some useful info in modeline.

(display-time-mode t)
(line-number-mode t)
(column-number-mode t)

;; Highlight matching parenthesis or bracket.

(show-paren-mode t)

;; Enable ido-mode for opening files and folders.
;; Cycle through results C-s and C-r. Disable with C-e.

(ido-mode t)
(ido-everywhere t)
(setq ido-show-dot-for-dired t)
(setq ido-flex-matching t)

;; Prevent clutter by not adding newly installed packages to init.el.
;; Your packages will still be loaded automatically.

(el-patch-defun package--save-selected-packages (&optional value)
  "Set and save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value))
  (el-patch-remove
    (if after-init-time
        (let ((save-silently inhibit-message))
          (customize-save-variable 'package-selected-packages package-selected-packages))
      (add-hook 'after-init-hook #'package--save-selected-packages))))

;; Make backspace work like you would expect when searching,
;; i.e. delete a character from the search string instead of going to the
;; previous result.

(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; Hide dotfiles in dired by default. Toggle using `C-c h`.

(defun dired-hidden-files-hook ()
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map (kbd "C-c h") #'dired-hide-dotfiles-mode))
(add-hook 'dired-mode-hook #'dired-hidden-files-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Package configuration and key bindings.
;;;

;; Slime: Interactive lisp shell.

(setq inferior-lisp-program "/usr/bin/sbcl") ; path to lisp binary
(setq slime-contribs '(slime-fancy))

;; Smex: Replaces M-x with an Ido-like interface.
;; Cycle through results C-s and C-r.

(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)
(setq smex-save-file "~/.emacs.d/plugin-data/smex/smex-items")

;; js2-mode: Improved JS major mode.

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; js-doc: Generate oilerplate JSDoc comments.

(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; nodejs-repl: interactive nodejs REPL.
;; Load current file with C-c r.

(defun nodejs-repl-load-buffer-file ()
  (interactive)
  (let ((fn (buffer-file-name )))
    (nodejs-repl-load-file fn)))

(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-cr" 'nodejs-repl-load-buffer-file)))
     

;; eslint-fix: Show eslint warnings when saving Javascript files.
;; eslint needs to be installed on the system.

(add-hook
 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indentation and language specific settings.
;;;

;; Prefer 2 spaces for indentation. Display tabs 2 spaces wide.

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Mode-specific indentation settings.

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq js-indent-level 2)

;; Show style warnings in js files. (requires eslint)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Themes and fonts.
;;;

;; Font. Change "Dejavu Sans Mono" to your favourite font.

(set-face-attribute 'default nil
                    :font "DejaVu Sans Mono"
                    :foreground "#c5c8c6"
                    :background "#222222"
                    :height 115)

;; Allow use of custom themes.

(setq custom-safe-themes t)

;; Custom theme directory.

(setq custom-theme-directory "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; Set theme to 'Tomorrow Night'. <http://github.com/chriskempson>

(require 'color-theme-tomorrow)
(color-theme-tomorrow--define-theme night)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global keybindings.
;;;

;; Open an eshell in a new window below the current one with C-c C-t.

(defun open-terminal-below ()
  "Split the window vertically and open a terminal below it."
  (interactive)
  (progn (split-window-below)
         (other-window 1)
         (eshell)))
(global-set-key (kbd "C-c t") 'open-terminal-below)

;; Switch windows with C-c C-[h/j/k/l] (vim-like movement)

(global-set-key (kbd "C-c C-h") 'windmove-left)
(global-set-key (kbd "C-c C-l") 'windmove-right)
(global-set-key (kbd "C-c C-k") 'windmove-up)
(global-set-key (kbd "C-c C-j") 'windmove-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Custom elisp files
;;;

;; path for elisp files

(add-to-list 'load-path "~/.emacs.d/elisp")

;; auto-scroll-mode: enable autoscroll in a buffer

(require 'auto-scroll)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Space for autogenerated code.
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-strict-trailing-comma-warning nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
