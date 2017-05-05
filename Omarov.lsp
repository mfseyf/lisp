;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Омаров Темирхан, группа 324;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Перевод ФАЛ в ДНФ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Внутренне представление:
;;;;переменная - (a, i),	a - сам атом,	i=0 - отрицание атома, i=1 иначе
;;;;;ДНФ - список списков переменных
;;;;;Конъюнкт - список переменных

;;;; Операции и константы
;;;;  Импликация   ->
;;;;  Дизъюнкция   ||
;;;;  Конъюнкция  &&
;;;;  Отрицание  !
;;;;  Константы true, false

;;;;  На вход: операции и операнды должны быть разделены пробелами. Всё выражение должно быть обернуто в список.
;;;;  На выходе: ДНФ - конъюнкты разделены ||. Знаки конъюнкций в конъюнктах опущены. Знак отрицания относится только к ближайшей справа переменной 

;входит ли атом а в список l хоть на одном его уровне
(defun deepmember (a l)
	(cond 	((null l) nil)
		((listp (car l)) (or (deepmember a (car l)) (deepmember a (cdr l))))
		((eql (car l) a) t)
		(t (deepmember a (cdr l)))))

;оборачиваем скобками операнды импликации
(defun skobimp (l)
	(skobimp1 l nil))
	
(defun skobimp1 (l kop)
	(cond	((null l) (list kop))
		((eql (car l) '->) (append (list kop '->) (skobimp1 (cdr l) nil)))
		((listp (car l)) (skobimp1 (cdr l)(append kop (list (delsk(skobimp (car l)))))))
		(t (skobimp1 (cdr l)(append kop (list (car l)))))))

;перевол импликации в базис. Н-р:  (e1) -> (e2)    =>     !(e1) || (e2)
(defun imptobasis (l)
	(cond	((null l) nil)
		((atom l) l)
		((not(deepmember '-> l)) l)
		((null (cdr l)) (cond 	((listp (car l)) (imptobasis (car l)))
					(t l)))
		((eql (cadr l) '->) (imptobasis (cons (list '! (car l) '|| (caddr l)) (cdddr l))))
		(t (cons (delsk(imptobasis(car l))) (imptobasis (cdr l))))))
		
;удаляет лишние вторые скобки	((е1))	=>	(e1)
(defun delsk (l)
	(cond	((atom l) l)
		((not (null (cdr l))) l)
		(t (car l))))

;снятие двойного отрицания	!!(е1)  	=>	(e1)
(defun dblneg (l)
	(cond	((null l) nil)
		((listp (car l)) (cons (dblneg (car l)) (dblneg (cdr l))))
		((null (cdr l)) l)
		((and (eql (car l) '!) (eql (cadr l) '!)) (dblneg (cddr l)))
		(t (cons (car l) (dblneg (cdr l))))))

;внесение отрицания в список, используя законы Де Моргана
(defun demorg (l)
	(cond	((null l) nil)
		((listp (car l)) (cons (demorg (car l)) (demorg (cdr l))))
		((null (cdr l)) l)
		((eql (car l) '!) (cond	((listp (cadr l)) (cons (demorg(notlist (cadr l))) (demorg (cddr l))))
					(t (cons (car l) (cons (cadr l) (demorg (cddr l)))))))
		(t (cons (car l) (demorg (cdr l))))))
		
;отрицание списка
(defun notlist (l)
	(cond	((null l) nil)
		((atom l) (list '! l))
		((= (length l) 2) (cond ((eql (car l) '!) (cadr l))
					(t 'ERRORNOTLIST)))
		((= (length l) 1) (cond ((atom l) 'NEVER)
					(t (notlist (car l)))))
;		((member '|| l) (demorgor (print (skobor l))))
		((member '|| l) (demorgor (skobor l)))
		((member '&& l)	(demorgand (skoband l)))
		(t 'ERROROPERATION)))

;!((e1) || (e2))	=>	(!(e1)&&!(e2))
(defun demorgor (l)
	(cond	((null l) nil)
;		((listp (car l)) (cons (print (notlist (print (car l)))) (demorgor (cdr l))))
		((listp (car l)) (cons (notlist (car l)) (demorgor (cdr l))))
		(t (cons '&& (demorgor (cdr l))))))

;!((e1) && (e2))	=>	(!(e1)||!(e2))
(defun demorgand (l)
	(cond	((null l) nil)
		((eql (car l) '&&) (cons '|| (demorgand (cdr l))))
;;		((null (cdr l)) (notlist (car l)))
		(t (cons (notlist (car l)) (demorgand (cdr l))))))

;оборачивание скобками операндов логического или
(defun skobor (l)
	(skobor1 l nil))
	
(defun skobor1 (l kop)
	(cond	((null l) (list kop))
		((eql (car l) '||) (append (list kop '||) (skobor1 (cdr l) nil)))
		((listp (car l)) (skobor1 (cdr l)(append kop (list (delsk(skobor (car l)))))))
		(t (skobor1 (cdr l)(append kop (list (car l)))))))	

;оборачивание скобками операндов логического или
(defun skoband (l)
	(skoband1 l nil))
	
(defun skoband1 (l kop)
	(cond	((null l) (list kop))
		((eql (car l) '&&) (append (list kop '&&) (skoband1 (cdr l) nil)))
		((listp (car l)) (skoband1 (cdr l)(append kop (list (delsk(skoband (car l)))))))
		(t (skoband1 (cdr l)(append kop (list (car l)))))))	
		
;перевод выражения во внутреннее представление.	Представляем каждую букву парой (а, i), где  а сама буква, i = 0, если буква отрицается. i = 1 иначе
(defun intrep (l)
	(cond	((null l) l)
		((listp (car l)) (cons (delsk(intrep (car l))) (intrep (cdr l))))
		((and (eql (car l) '!) (atom (cadr l))) (cons (cons (cadr l) 0) (intrep (cddr l))))
		((or (eql (car l) '&&) (eql (car l) '||)) (cons (car l) (intrep (cdr l))))
		(t (cons (cons (car l) 1) (intrep (cdr l))))))

; данное S-выражение - буква (переменная)?
(defun valp (l)
	(cond	((null l) nil)
		((numberp (cdr l)) t)
		(t nil)))
		
;список не является переменной, а выражением 
(defun exprp (l)
	(cond	((and (not (valp l)) (listp l)) t)
		(t nil)))
		
;дистрибутивность коньюнкции относительно дизьюнкции   (а+с)*у = а*у + с*у
(defun distrib (l)
	(cond	((null l) nil)
		((valp l) l)
		((null (cdr l)) (cond 	((valp (car l)) (car l))
;					((exprp(car l)) (distrib (print(car l))))
					((exprp(car l)) (distrib (car l)))
					( t 'ERRORDISTRIB)))
;		((eql (cadr l) '&&) 	(cond	((exprp (car l)) (distrib (print(cons (conjunct (car l)(caddr l)) (cdddr l)))))
;						((exprp (caddr l)) (distrib (print(cons (conjunct (caddr l) (car l)) (cdddr l)))))
		((eql (cadr l) '&&) 	(cond	((exprp (car l)) (distrib (cons (conjunct (car l)(caddr l)) (cdddr l))))
						((exprp (caddr l)) (distrib (cons (conjunct (caddr l) (car l)) (cdddr l))))
						(t (cons (car l) (cons (cadr l) (list(distrib (cddr l))))))))
		((eql (cadr l) '||) 	(list (distrib (car l)) '|| (distrib (cddr l))))
		(t 'CANTBETRUTH)))
	
;внесение  у в список (а+с)*у 	=>	 (а*у + с*у)
(defun conjunct (e x)
	(conj e x (list x '&&)))

(defun conj (e x kop)
	(cond 	((null e) (list kop))
		((eql (car e) '||) (append (list kop '||) (conj (cdr e) x (list x '&&))))
		(t (conj (cdr e) x (append kop (list (car e)))))))
		
; делаем список одноуровневым (можно, потому что все коньюнкции встречаются раньше дизьюнкции
(defun flatten (l)
	(cond	((null l) l)
		((or (atom l) (valp l)) (cons l nil))
		(t (append (flatten (car l)) (flatten (cdr l))))))

; Для удобного представления избавляемся от &&
(defun delamp (l)
	(cond 	((null l) l)
		((eql (car l) '&&) (delamp (cdr l)))
		(t (cons (car l) (delamp (cdr l))))))
		
; Оборачиваем в списки конъюнкты
(defun dnf (l)
	(dnf1 l nil))
	
(defun dnf1 (l kop)
	(cond	((null l) (list kop))
		((eql (car l) '||) (append (list kop '||) (dnf1 (cdr l) nil)))
		;((listp (car l)) (skobor1 (cdr l)(append kop (list (delsk(skobor (car l)))))))
		(t (dnf1 (cdr l)(append kop (list (car l)))))))	

; Для удобного представления избавляемся от ||
(defun delor (l)
	(cond 	((null l) l)
		((eql (car l) '||) (delor (cdr l)))
		(t (cons (car l) (delor (cdr l))))))
		
;Удаляем конъюнкты, содержащие False, !True, val * ! val
(defun delfalse (l)
	(cond	((null l) l)
		((memberfalse (car l)) (delfalse (cdr l)))
		((membervnotv (car l)) (delfalse (cdr l)))
		(t (cons (car l) (delfalse (cdr l))))))
	
;В конъюнкт входит False, !True ?
(defun memberfalse (l)
	(cond	((null l) nil)
		((or (eqval (car l) '(true . 0)) (eqval (car l) '(false . 1))) t)
		(t (memberfalse (cdr l)))))
		
; Одинаковы ли буквы (переменные) v и w
(defun eqval (v w)
	(and (eql (car v) (car w)) (eql (cdr v) (cdr w))))
	
;В конъюнкт входит val * ! val 
(defun membervnotv (l)
	(cond	((null l) nil)
		((memvnv (car l) (cdr l)) t)
		(t (membervnotv (cdr l)))))
; Входит ли !v в l 
(defun memvnv (v l)
	(cond	((null l) nil)
		((eqval (cons (car v) (- 1 (cdr v))) (car l)) t)
		(t (memvnv v (cdr l)))))

;Удаляем повторные вхождения букв в конъюнктах в днф
(defun deleqvals (l)
	(cond 	((null l) l)
		(t (cons (deleqv (car l)) (deleqvals (cdr l))))))
	
;Удаляем повторные вхождения букв в конъюнкт
(defun deleqv (l)
	(cond 	((null l) nil)
		((memberval (car l) (cdr l)) (deleqv (cdr l)))
		(t (cons (car l) (deleqv (cdr l))))))

(defun deltrue (l)
	(cond	((null l) l)
		(t (cons (deltr (car l)) (deltrue (cdr l))))))
		
(defun deltr (l)
	(cond	((null l) nil)
		((or (eqval (car l) '(true . 1)) (eqval (car l) '(false . 0))) (deltr (cdr l)))
		(t (cons (car l) (deltr (cdr l))))))

;Входит ли v в l
(defun memberval (v l)
	(cond	((null l) nil)
		((eqval v (car l)) t)
		(t (memberval v (cdr l)))))

;Перевод днф из внутреннего представления в итоговое
(defun convert (l)
	(cond	((null l) nil)
		(t (cons (dellastand(makeconj (car l))) (cons '|| (convert (cdr l)))))))

;Перевод конъюнкта из внутреннего представления в итоговое
(defun makeconj (l)
	(cond	((null l) nil)
		((= 0 (cdar l))	(cons '! (cons (caar l) (cons '&& (makeconj (cdr l))))))
		(t (cons (caar l) (cons '&& (makeconj (cdr l)))))))

;Удаляем лишний появившийся знак конъюнкции		
(defun dellastand (l)
	(cond	((null l) nil)
		((and(null (cdr l))(eql (car l) '&&)) nil)
		(t (cons (car l) (dellastand (cdr l))))))

		
;Удаляем лишний появившийся знак дизъюнкции		
(defun dellastor (l)
	(cond	((null l) nil)
		((and(null (cdr l))(eql (car l) '||)) nil)
		(t (cons (car l) (dellastor (cdr l))))))

(defun main (l)
	(flatten(dellastor(convert(deltrue(deleqvals(delfalse (delor(dnf(delamp(flatten(distrib(distrib(intrep(demorg(dblneg (delsk(imptobasis(skobimp l)))))))))))))))))))

;(print(deleqvals(delfalse (delor(dnf(delamp(flatten(distrib(distrib(intrep(demorg(dblneg (delsk(imptobasis(skobimp '((a -> (b || c -> d) -> f) && g))))))))))))))))
;(print(main '((a -> (b || c -> d) -> f) && g)))
;(print(main '((a && ! a -> b && ! b -> c && ! c)))
;(print(deleqvals(delfalse (delor(dnf(delamp(flatten(distrib(distrib(intrep(demorg(dblneg (delsk(imptobasis(skobimp '(a && ! a -> b && ! b -> c && ! c))))))))))))))))
;(print(intrep(print(demorg(print(dblneg (delsk(imptobasis(skobimp '(a && ! a -> b && ! b -> c && ! c))))))))))
;(print(main '(a && ! a -> b && ! b -> c && ! c)))
;(print(main '((a -> (b || c -> d) -> f) && g)))
(print(main '((! a || d || false) -> !(b && ! c || d))))