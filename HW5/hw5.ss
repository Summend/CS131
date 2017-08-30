(define (null-ld? obj)
  (equal? (car obj) (cdr obj)))

(define listdiff?
  (lambda (obj)
  (if (pair? obj)
	(if (empty? (cdr obj))
		#t
		(if (empty? (car obj))
			#f
			(if (equal? (car obj) (cdr obj))
				(if (eq? (car obj) (cdr obj))
				#t #f)
				(listdiff? (cons (cdr (car obj)) (cdr obj)))
	)))
	#f)))

(define cons-ld 
(lambda (obj listdiff)
	(if (listdiff? listdiff)
	(cons (cons obj (car listdiff)) (cdr listdiff))
	(error 'ERROR))))

(define car-ld 
	(lambda (listdiff)
	(if (null-ld? listdiff)
		(error 'ERROR)
		(if (listdiff? listdiff)
			(car (car listdiff))
			(error 'ERROR)))))

(define cdr-ld 
	(lambda (listdiff)
	(if (null-ld? listdiff)
		(error 'ERROR)
		(if (listdiff? listdiff)
			(cons (cdr (car listdiff)) (cdr listdiff))
			(error 'ERROR)))))

(define listdiff
	(lambda (obj . args)
	(cons (cons obj args) '())))

(define length-ld
	(lambda (listdiff)
		(if (listdiff? listdiff)
			(if (equal? (car listdiff) (cdr listdiff)) 0
			(+ 1 (length-ld (cons (cdr (car listdiff)) (cdr listdiff)))))
		(error 'Error))))

(define append-ld
	(lambda (listdiff . args)
		(cons (cons args (car listdiff)) (cdr listdiff))))

(define assq-ld
	(lambda (obj alistdiff)
	(if (empty? (car alistdiff))
		#f
		(if (equal? obj (car (car (car alistdiff))))
			(car (car alistdiff))
			(assq-ld obj (cons (cdr (car alistdiff)) (cdr alistdiff)))))))

(define list->listdiff
	(lambda (list)
		(if (empty? list)
		'()
		(cons (first list) (list->listdiff (rest list))))))

(define listdiff->list
	(lambda (listdiff)
	(if (equal? (car listdiff) (cdr listdiff))
		'()
		(list* (car (car listdiff)) (listdiff->list (cons (cdr (car listdiff)) (cdr listdiff)))))))

(define expr-returning
	(lambda (listdiff)
	`(cons (car ',listdiff) (car ',listdiff))))

(define ils (append '(a e i o u) 'y))
(define d1 (cons ils (cdr (cdr ils))))
(define d2 (cons ils ils))
(define d3 (cons ils (append '(a e i o u) 'y)))
(define d4 (cons '() ils))
(define d5 0)
(define d6 (listdiff ils d1 37))
(define d7 (append-ld d1 d2 d6))
(define e1 (expr-returning d1))
(define kv1 (cons d1 'a))
(define kv2 (cons d2 'b))
(define kv3 (cons d3 'c))
(define kv4 (cons d1 'd))
(define d8 (listdiff kv1 kv2 kv3 kv4))
