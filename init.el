;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General editor settings.
;;;

;;; Enable the MELPA repository on Emacs versions 24 or newer.
;;; View packages with `M-x list-packages`.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Autoload installed packages.
(package-initialize)

;;; Autoload files in elisp dir.
(defvar autoload-path "~/.emacs.d/elisp/")
(add-to-list 'load-path autoload-path)
(mapc 'load (mapcar 'file-name-base
                    (directory-files autoload-path t "\.el$")))

;;; Remove window decorations and general bling.
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;;; Initialize environment from the user’s shell.
(exec-path-from-shell-initialize)

;;; Put temporary files in tmp directory so as to not clutter working directory.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

;;; Make emacs prompt less for file actions.
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

;;; Don't ask to kill buffers with active processes. (Annoying.)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;; Display some info in modeline.
(display-time-mode t)
(line-number-mode t)
(column-number-mode t)

;;; Highlight matching parenthesis or bracket.
(show-paren-mode t)

;;; Enable 'ido-mode' for opening files and folders.
;;; Cycle through results C-s and C-r. Disable with C-e.
(ido-mode t)
(ido-everywhere t)
(setq ido-show-dot-for-dired t)
(setq ido-flex-matching t)


;;; Prevent clutter by not adding newly installed packages to init.el.
;;; Your packages will still be loaded automatically.
(el-patch-defun package--save-selected-packages (&optional value)
  "Set and save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value))
  (el-patch-remove
    (if after-init-time
        (let ((save-silently inhibit-message))
          (customize-save-variable 'package-selected-packages package-selected-packages))
      (add-hook 'after-init-hook #'package--save-selected-packages))))

;;; Make backspace work like you would expect when searching,
;;; i.e. delete a character from the search string instead of going to the
;;; previous result.
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;;; Hide dotfiles in dired by default. Toggle using `C-c h`.
(require 'dired)
(defun dired-hidden-files-hook ()
  "Press \\(kbd \"C-c h\") to how hidden files whilw in 'dired-mode'."
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map (kbd "C-c h") #'dired-hide-dotfiles-mode))
(add-hook 'dired-mode-hook #'dired-hidden-files-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;h;;;;;;;;;;;
;;;
;;; Package configuration and key bindings.
;;;

;;; Enable flychecking syntax globally.
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;;; Slime: Interactive lisp shell.
(defvar inferior-lisp-program "/usr/bin/sbcl") ; path to lisp binary
(setq slime-contribs '(slime-fancy))

;;; Smex: Replaces M-x with an Ido-like interface.
;;; Cycle through results C-s and C-r.
(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)
(defvar smex-save-file "~/.emacs.d/plugin-data/smex/smex-items")

;;; js2-mode: Improved JS major mode.
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;; js-doc: Generate oilerplate JSDoc comments.
(require 'js2-mode)
(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;;; nodejs-repl: interactive nodejs REPL.
;;; Load current file with C-c r.
(defun nodejs-repl-load-buffer-file ()
  "Load the current buffers file in noodejs-repl."
  (interactive)
  (let ((fn (buffer-file-name )))
    (nodejs-repl-load-file fn)))
(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-cr" 'nodejs-repl-load-buffer-file)))

;;; nodejs-babel: node REPL with babel features.
(defun babel-repl-load-file (file)
  "Load FILE in nodejs-repl."
  (interactive (list (read-file-name "Load file: " nil nil 'lambda)))
  (let ((proc (nodejs-repl--get-or-create-process)))
    (comint-send-string proc (format ".load %s\n" file))))


;;; Don’t use flycheck for JS or JSON files.
(require 'flycheck)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))


;;; eslint-fix: Show eslint warnings when saving Javascript files.
;;; eslint needs to be installed on the system.
(add-hook
 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indentation and language specific settings.
;;;

;;; Prefer 2 spaces for indentation. Display tabs 2 spaces wide.
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;;; Mode-specific indentation settings.
;;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Indentation for web-mode."
  (defvar web-mode-markup-indent-offset 2)
  (defvar web-mode-css-indent-offset 2)
  (defvar web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(defvar js-indent-level 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Themes and fonts.
;;;

;;; Font. Change "Dejavu Sans Mono" to your favourite font.
(set-face-attribute 'default nil
                    :font "DejaVu Sans Mono"
                    :foreground "#c5c8c6"
                    :background "#222222"
                    :height 115)

;;; Allow use of custom themes.
(setq custom-safe-themes t)

;;; Custom theme directory.
(add-to-list 'load-path "~/.emacs.d/themes")

;;; Set theme to 'Tomorrow Night'. <http://github.com/chriskempson>
(require 'color-theme-tomorrow)
(color-theme-tomorrow--define-theme night)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global keybindings.
;;;

;;; Open an eshell in a new window below the current one with C-c C-t.
(defun open-terminal-below ()
  "Split the window vertically and open a terminal below it."
  (interactive)
  (progn (split-window-below)
         (other-window 1)
         (eshell)))
(global-set-key (kbd "C-c t") 'open-terminal-below)

;;; Switch windows with C-c C-[h/j/k/l] (vim-like movement)
(global-set-key (kbd "C-c C-h") 'windmove-left)
(global-set-key (kbd "C-c C-l") 'windmove-right)
(global-set-key (kbd "C-c C-k") 'windmove-up)
(global-set-key (kbd "C-c C-j") 'windmove-down)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Space for code generated by customize commands.
;;;

(custom-set-variables
 ;;; custom-set-variables was added by Custom.
 ;;; If you edit it by hand, you could mess it up, so be careful.
 ;;; Your init file should contain only one such instance.
 ;;; If there is more than one, they won't work right.
 '(erc-interpret-controls-p t)
 '(erc-interpret-mirc-color t)
 '(erc-notifications-icon nil)
 '(erc-notifications-mode t)
 '(erc-notify-mode t)
 '(js2-strict-trailing-comma-warning nil)
 '(nodejs-repl-command "babel-node"))
(custom-set-faces
 ;;; custom-set-faces was added by Custom.
 ;;; If you edit it by hand, you could mess it up, so be careful.
 ;;; Your init file should contain only one such instance.
 ;;; If there is more than one, they won't work right.
 )

;;;; init.el ends here.
