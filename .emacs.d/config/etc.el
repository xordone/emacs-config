;; Кусок кода ниже позволяет позволяет использовать хоткеи с русской раскладкой.
;; Найден в https://www.linux.org.ru/forum/general/9959057?lastmod=1422960214493#comment-11282246
;; Спасибой добрый дядя )))

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
	(modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
	(let* ((to (car map))
	       (from (quail-get-translation
		      (cadr map) (char-to-string to) 1)))
	  (when (and (characterp from) (characterp to))
	    (dolist (mod modifiers)
	      (define-key local-function-key-map
		(vector (append mod (list from)))
		(vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'russian-computer)



  ;; Сообщение в буфере scratch
(setq initial-scratch-message "Добро пожаловать в буфер *scratch* !
    Hotkeys for learning:
C-h k - Keybindings
C-h f - functions
C-h v - variables
C-h a - apropos
    Links:

http://www.gnu.org/software/emacs/tour/
http://ergoemacs.org/emacs/elisp.html

Если ты в серьез собрался его использовать,
то не забудь сохранить его в итоге.
M-A-s или C-x C-s




")
