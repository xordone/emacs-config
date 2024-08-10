;;
;;; Навигация
;;;; Крестовина
;;;;; Вверх
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i") 'previous-line)

;;;;; Вниз
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'next-line)

;;;;; Влево
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'backward-char)

;;;;; Вправо
(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-l") 'forward-char)

;;;;; Page Up
(global-unset-key (kbd "M-I"))
(global-set-key (kbd "M-I") 'scroll-down-command)

;;;;; Page Down
(global-unset-key (kbd "M-K"))
(global-set-key (kbd "M-K") 'scroll-up-command)

;;;;; Forward word
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'forward-word)

;;;;; Backward word
(global-unset-key (kbd "M-u"))
(global-set-key (kbd "M-u") 'backward-word)

;;;;; Beginnning of line
(global-unset-key (kbd "M-h"))
(global-set-key (kbd "M-h") 'move-beginning-of-line)

;;;;; End of line
(global-unset-key (kbd "M-;"))
(global-set-key (kbd "M-;") 'move-end-of-line)

;;;;; End of buffer
(global-unset-key (kbd "M-L"))
(global-set-key (kbd "M-L") 'end-of-buffer)

;;;;; Beginning of buffer
(global-unset-key (kbd "M-J"))
(global-set-key (kbd "M-J") 'beginning-of-buffer)

;;;; Редактирование
;;;;; Выделение
(global-unset-key (kbd "M-SPC"))
(global-set-key (kbd "M-SPC") 'set-mark-command)

;;;;; Копирование
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c") 'kill-ring-save)

;;;;; Вырезание
(global-unset-key (kbd "s-x"))
(global-set-key (kbd "s-x") 'kill-region)

;;;;; Вставка
(global-unset-key (kbd "M-v"))
(global-set-key (kbd "M-v") 'yank)

;;;;; Отмена
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z") 'undo)

;;;;; Сохранение
(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") 'save-buffer)

;;;;; Открытие
(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'find-file)

;;; Окна
;;;; Вверх
(global-unset-key (kbd "C-M-i"))
(global-set-key (kbd "C-M-i") 'windmove-up)
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'windmove-up)

;;;; Вниз
(global-unset-key (kbd "C-M-k"))
(global-set-key (kbd "C-M-k") 'windmove-down)

;;;; Влево
(global-unset-key (kbd "C-M-j"))
(global-set-key (kbd "C-M-j") 'windmove-left)

;;;; Вправо
(global-unset-key (kbd "C-M-l"))
(global-set-key (kbd "C-M-l") 'windmove-right)

;;;; Ресайз
(defun my-enlarge-vert ()
  (interactive)
  (enlarge-window 2))

(defun my-shrink-vert ()
  (interactive)
  (enlarge-window -2))

(defun my-enlarge-horz ()
  (interactive)
  (enlarge-window-horizontally 2))

(defun my-shrink-horz ()
  (interactive)
  (enlarge-window-horizontally -2))


(global-unset-key (kbd "C-("))
(global-set-key (kbd "C-(") 'my-shrink-vert)
(global-unset-key (kbd "C-)"))
(global-set-key (kbd "C-)") 'my-enlarge-vert)
(global-unset-key (kbd "C-9"))
(global-set-key (kbd "C-9") 'my-shrink-horz)
(global-unset-key (kbd "C-0"))
(global-set-key (kbd "C-0") 'my-enlarge-horz)


(defun my-50%-horz ()
  (interactive)
  (let* ((width (round (* (frame-width) 0.5)))
	 (cur-width (window-width))
	 (delta (- width (+ cur-width 5))))
    (enlarge-window-horizontally delta)))

(defun my-50%-vert ()
  (interactive)
  (let* ((height (round (* (frame-height) 0.5)))
	 (cur-height (window-height))
	 (delta (- height (+ cur-height 5))))
    (enlarge-window delta)))

(defvar *larg-window-size-percent* 0.7)

(defun my-50%-horz ()
  (interactive)
  (let* ((width (round (* (frame-width) 0.5)))
	 (cur-width (window-width))
	 (delta (- width (+ cur-width 5))))
    (enlarge-window-horizontally delta)))

(defun my-50%-vert ()
  (interactive)
  (let* ((height (round (* (frame-height) 0.5)))
	 (cur-height (window-height))
	 (delta (- height (+ cur-height 5))))
    (enlarge-window delta)))

(defun my-super-enlarge-horz ()
  (interactive)
  (let* ((width (round (* (frame-width) *larg-window-size-percent*)))
	 (cur-width (window-width))
	 (delta (- width cur-width)))
    (enlarge-window-horizontally delta)))

(defun my-super-enlarge-vert ()
  (interactive)
  (let* ((height (round (* (frame-height) *larg-window-size-percent*)))
	 (cur-height (window-height))
	 (delta (- height cur-height)))
    (enlarge-window delta)))

(defun my-super-shrink-horz ()
  (interactive)
  (let* ((width (round (* (frame-width) (- 1 *larg-window-size-percent*))))
	 (cur-width (window-width))
	 (delta (- width cur-width)))
    (enlarge-window-horizontally delta)))

(defun my-super-shrink-vert ()
  (interactive)
  (let* ((height (round (* (frame-height) (- 1 *larg-window-size-percent*))))
	 (cur-height (window-height))
	 (delta (- height cur-height)))
    (enlarge-window delta)))


(global-unset-key (kbd "M-("))
(global-set-key (kbd "M-(") 'my-super-shrink-vert)
(global-unset-key (kbd "M-)"))
(global-set-key (kbd "M-)") 'my-super-enlarge-vert)
(global-unset-key (kbd "M-9"))
(global-set-key (kbd "M-9") 'my-super-shrink-horz)
(global-unset-key (kbd "M-0"))
(global-set-key (kbd "M-0") 'my-super-enlarge-horz)
(global-unset-key (kbd "M-8"))
(global-set-key (kbd "M-8") 'my-50%-horz)
(global-unset-key (kbd "M-*"))
(global-set-key (kbd "M-*") 'my-50%-vert)

(global-set-key (kbd "M-<f12>") 'bookmark-set)
(global-set-key (kbd "<f12>") 'bookmark-bmenu-list)
