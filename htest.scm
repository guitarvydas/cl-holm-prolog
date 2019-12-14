(define db
  '(((man nils))
    ((mortal (? x)) (man (? x)))))

(define g
  '((mortal (? x))))

(define (htest-1)
  (let ((fb '(((man nils)))))
                   (prove6 '() '((man nils)) fb empty 1 '()))) 
(define (htest-2)
  (let ((fb '(((man nils)))))
                   (prove6 '() '((aardvark nils)) fb empty 1 '()))) 
(define (htest0)   (prove6 '() '((man nils)) db empty 1 '() ))
(define (htest1)   (prove6 '() '((man (? m))) db empty 1 '()))
(define (htest2)   (prove6 '() '((mortal nils)) db empty 1 '()))
(define (htest3)   (prove6 '() '((mortal (? m))) db empty 1 '()))
