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

;; Benchmark Startup

(require 'benchmark-init)
(benchmark-init/activate)

;; Remove window decorations and general bling.

(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Get the environment from user's shell.
;; Useful for getting Emac's eshell to read from your $PATH.

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

(display-time-mode t)  ; Time
(line-number-mode t)   ; Line number
(column-number-mode t) ; Column number

;; Highlight matching parenthesis or bracket.

(show-paren-mode t)

;; Use ido-mode for finding files and folders quickly.
;; It has fuzzy search. Example: `M-x mgstus` will likely run magit-status.
;; C-s to go forward in list, C-r to to backwards.
;; Press C-e while in Ido to disable it temporarily.

(ido-mode t)
(ido-everywhere t)
(setq ido-show-dot-for-dired t)

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
;;; Installed packages & their configuration.
;;;

;; Slime is a REPL for lisp. Assumes you have SBCL but any LISP
;; implementation works.

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Smex is like Ido but for running commands. Has fuzzy search.

(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)
(setq smex-save-file "~/.emacs.d/plugin-data/smex/smex-items")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indentation and language specific settings.
;;;

;; Use spaces over tabs.

(setq-default indent-tabs-mode nil)

;; Display tab character as 2 spaces wide. (They are still tabs.)

(setq tab-width 2)

;; Mode-specific indentation settings.

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq js-indent-level 2)

;; Use ESLINT for Javascript if installed on the system.

(eval-after-load 'js-mode
  '(add-hook
    'js-mode-hook
    (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))
(eval-after-load 'js2-mode
  '(add-hook
    'js2-mode-hook
    (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Themes and fonts.
;;;

;; Treat all themes as safe
;; Caution: Themes can execute arbitrary code.

(custom-set-variables
 '(custom-safe-themes t))

;; Custom theme directory. If you download a theme from the internet, place
;; it's .el file here. Only use themes from trusted sources.

(setq custom-theme-directory "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; In GUI Emacs, use Tomorrow Night theme by Chris Kempson.

(when (display-graphic-p)
  (require 'color-theme-tomorrow)
  (color-theme-tomorrow--define-theme night))

;; Font & settings. Add your favourite font instead of DejaVu Sans Mono.

(set-face-attribute 'default nil
                    :font "DejaVu Sans Mono"
                    :foreground "#c5c8c6"
                    :background "#222222"
                    :height 105)
