;;; buffer-local-keys.el --- Define a key binding for the active buffer.

;;;
;;; Commentary:
;;;
;;; Provides `buffer-local-set-key'.
;;;

;;; Code:

(defun buffer-local-set-key (key func)
  "Create a buffer-local key binding using KEY and FUNC."
  (interactive "KSet key on this buffer: \naCommand: ")
  ;; Make a temporary minor mode for the key binding.
  (let ((name (format "%s-temp" (file-name-sans-extension (buffer-name)))))
    (eval `(define-minor-mode ,(intern name)
             ""))
    ;; Make a key map for the mode.
    (let* ((mapname (format "%s-map" name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval `(define-key ,map ,key func)))
    ;; Enable the minor mode
    (funcall (intern name) t)
    ;; Move it to the top of `minor-mode-alist' so it gains priority.
    ;;(add-to-list 'minor-mode-alist `(,(make-symbol name) "Local-Key"))
    )
  )

(setq minor-mode-alist (delete (format "%s-temp" (file-name-sans-extension (buffer-name))) minor-mode-alist))
(defun strip-duplicates (list)
  (let ((new-list nil))
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

; minor-mode-alist

(delete 'minor-mode-alist 'buffer-local-keys-temp)

(provide 'buffer-local-keys)

;;; buffer-local-keys.el ends here

