;; https://debian.pro/1016
(setq russian-symbols '(
(?й . ?q)
(?ц . ?w)
(?у . ?e)
(?к . ?r)
(?е . ?t)
(?н . ?y)
(?г . ?u)
(?ш . ?i)
(?щ . ?o)
(?з . ?p)
(?х . ?\[)
(?ъ . ?\])
(?ф . ?a)
(?ы . ?s)
(?в . ?d)
(?а . ?f)
(?п . ?g)
(?р . ?h)
(?о . ?j)
(?л . ?k)
(?д . ?l)
(?ж . ?\;)
(?э . ?')
(?я . ?z)
(?ч . ?x)
(?с . ?c)
(?м . ?v)
(?и . ?b)
(?т . ?n)
(?ь . ?m)
(?б . ?,)
(?ю . ?.)
(?Й . ?Q)
(?Ц . ?W)
(?У . ?E)
(?К . ?R)
(?Е . ?T)
(?Н . ?Y)
(?Г . ?U)
(?Ш . ?I)
(?Щ . ?O)
(?З . ?P)
(?Х . ?{)
(?Ъ . ?})
(?Ф . ?A)
(?Ы . ?S)
(?В . ?D)
(?А . ?F)
(?П . ?G)
(?Р . ?H)
(?О . ?J)
(?Л . ?K)
(?Д . ?L)
(?Ж . ?:)
(?Э . ?\")
(?Я . ?Z)
(?Ч . ?X)
(?С . ?C)
(?М . ?V)
(?И . ?B)
(?Т . ?N)
(?Ь . ?M)
(?Б . ?<)
(?Ю . ?>)
(?Ё . ?~)
(?ё . ?`)
))
(setq russian-symbols-full (append russian-symbols
'((?. . ?/)
(?, . ??)
(?\" . ?@)
(?№ . ?#)
(?\; . ?$)
(?: . ?^)
(?\? . ?&))))
(defun cm-ru-to-en-string(string)
(apply 'concat (mapcar (lambda (arg) (setq arg (format "%c" (or (cdr (assoc arg russian-symbols-full)) arg)))) string)))
(defun cm-en-to-ru-string(string)
(apply 'concat (mapcar (lambda (arg) (setq arg (format "%c" (or (car (rassoc arg russian-symbols-full)) arg)))) string)))
(defun cm-ru-to-en-region()
(interactive)
(let ((text (buffer-substring-no-properties (mark) (point))))
(delete-region (mark) (point))
(insert (cm-ru-to-en-string text))))
(defun cm-en-to-tu-region()
(interactive)
(let ((text (buffer-substring-no-properties (mark) (point))))
(delete-region (mark) (point))
(insert (cm-en-to-ru-string text))))
(setq russian-symbols-map1
(append
(mapcar (lambda (arg) (setq arg (cons (+ (- ?\C-ы ?ы) (car arg)) (+ (- ?\C-s ?s) (cdr arg))))) russian-symbols)
(mapcar (lambda (arg) (setq arg (cons (+ (- ?\M-ы ?ы) (car arg)) (+ (- ?\M-s ?s) (cdr arg))))) russian-symbols)
(mapcar (lambda (arg) (setq arg (cons (+ (- ?\C-\M-ы ?ы) (car arg)) (+ (- ?\C-\M-s ?s) (cdr arg))))) russian-symbols)
(mapcar (lambda (arg) (setq arg (cons (+ (- ?\H-ы ?ы) (car arg)) (+ (- ?\H-s ?s) (cdr arg))))) russian-symbols)
(mapcar (lambda (arg) (setq arg (cons (+ (- ?\H-\C-ы ?ы) (car arg)) (+ (- ?\H-\C-s ?s) (cdr arg))))) russian-symbols)
(mapcar (lambda (arg) (setq arg (cons (+ (- ?\H-\M-ы ?ы) (car arg)) (+ (- ?\H-\M-s ?s) (cdr arg))))) russian-symbols)
(mapcar (lambda (arg) (setq arg (cons (+ (- ?\H-\C-\M-ы ?ы) (car arg)) (+ (- ?\H-\C-\M-s ?s) (cdr arg))))) russian-symbols)))
(setq russian-symbols-map2
(append
russian-symbols-map1
russian-symbols)) ; We must not start with russian letters, but if it is element in a sequence - in should be fine.
(setq symbols2 russian-symbols-map1) ; One-key sequence command.
(let ((symbols2 russian-symbols-map1))
(while symbols2
(if
(and (symbolp (lookup-key global-map
(vector
(cdr (car symbols2))
)))
(lookup-key global-map
(vector
(cdr (car symbols2))
)))
(global-set-key
(vector
(car (car symbols2))
)
(lookup-key global-map
(vector
(cdr (car symbols2))
))))
(setq symbols2 (cdr symbols2))))
(let ((symbols1 russian-symbols-map2) (symbols2 russian-symbols-map1)) ; Two keys sequence
(while symbols1
(while symbols2
(if
(and (symbolp (lookup-key global-map
(vector
(cdr (car symbols2))
(cdr (car symbols1))
)))
(lookup-key global-map
(vector
(cdr (car symbols2))
(cdr (car symbols1))
)))
(global-set-key
(vector
(car (car symbols2))
(car (car symbols1))
)
(lookup-key global-map
(vector
(cdr (car symbols2))
(cdr (car symbols1))
))))
(setq symbols2 (cdr symbols2)))
(setq symbols2 russian-symbols-map1)
(setq symbols1 (cdr symbols1))))
(provide 'shamanizm) ;russian emacs-users should lol reading this


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
