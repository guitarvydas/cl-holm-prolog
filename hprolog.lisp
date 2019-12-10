(in-package :cl-holm-prolog)

(defconstant +true+ t)
(defconstant +false+ nil)
(defparameter *db* nil)

(defun null? (x) (null x))
(defun pair? (x) (and (not (null x)) (or (listp x) (consp x))))
(defun eq? (x y) (eq x y))
(defun eqv? (x y) (eql x y))

(defun newline () (format *standard-output* "~%"))

(defun display (x) (format *standard-output* "~A" x))


(defmacro link (&rest x) `(list ,@x))
(defmacro L_l (x) `(car ,x))
(defmacro L_g (x) `(cadr ,x))
(defmacro L_r (x) `(caddr ,x))
(defmacro L_e (x) `(cadddr ,x))
(defmacro L_n (x) `(car (cddddr ,x)))

(defun L_c (x) (cadr (cddddr x)))


(defun clear_r (x)
  (rplaca (cddr x) '(())))
  ;(set-car! (cddr x) '(())))


(defun back6 (l g r e n c)
  (cond
   ((and (pair? g)
         (pair? r))
    (prove6 l g (cdr r) e n c))
   ((pair? l)
    (prove6 (L_l l) (L_g l) (cdr (L_r l)) (L_e l) (L_n l) (L_c l)))))

(defun prove6 (l g r e n c)
  (cond
   ((null? g)
    (print-frame e)
    (back6 l g r e n c))
   ((eq? :! (car g))
    (clear_r c)
    (prove6 c (cdr g) r e n c))
   ((eq? :r! (car g))
    (prove6 l (cddr g) r e n (cadr g)))
   ((null? r)
    (if (null? l)
        +true+
      (back6 l g r e n c)))
   (t
    (let* ((a  (copy (car r) n))
           (e* (unify (car a) (car g) e)))
      (if e*
          (prove6 (link l g r e n c)
                  (append (cdr a) `(:r! ,l) (cdr g))
                  *db*
                  e*
                  (+ 1 n)
                  l)
        (back6 l g r e n c))))))


(defparameter empty '((bottom)))

;(define var '?)
(defun name (x) (cadr x))
(defun htime (x) (cddr x))

(defun var? (x)
  (and (pair? x)
       (eq? :? (car x))))

;; (defun lookup (v e)
;;   (let ((id (name v))
;;         (tm  (htime v)))
;;     (labels ((tail-rec-loop (e) ;; Let Over Lambda shows how to do this in CL, with actual tail recursion
;;              (cond ((not (pair? (caar e)))
;;                     +false+)
;;                    ((and (eq? id (name (caar e)))
;;                          (eqv? tm (htime (caar e))))
;;                     (car e))
;;                    (t
;;                     (tail-rec-loop (cdr e))))))
;;       (tail-rec-loop e))))

(defun lookup (v orig-e)
  (let ((id (name v))
        (tm  (htime v)))
    (labels ((tail-rec-loop (ee) ;; Let Over Lambda shows how to do this in CL, with actual tail recursion
	       (loop for e = ee then (cdr e)
		    do
		    (cond ((not (pair? (caar e)))
			   (return-from tail-rec-loop +false+))
			  ((and (eq? id (name (caar e)))
				(eqv? tm (htime (caar e))))
			   (return-from tail-rec-loop (car e)))
			  (t nil)))))
      (tail-rec-loop orig-e))))

(defun value (x e)
  (if (var? x)
      (let ((v (lookup x e)))
        (if v
            (value (cadr v) e)
          x))
    x))

(defun copy (x n)
  (cond
   ((not (pair? x)) x)
   ((var? x) (append x n))
   (t
    (cons (copy (car x) n)
          (copy (cdr x) n)))))

(defun bind (x y e)
  (cons (list x y) e))

(defun unify (x y e)
  (let ((x (value x e))
        (y (value y e)))
    (cond
     ((eq? x y) e)
     ((var? x) (bind x y e))
     ((var? y) (bind y x e))
     ((or (not (pair? x))
          (not (pair? y))) +false+)
     (t
      (let ((e* (unify (car x) (car y) e)))
        (and e* (unify (cdr x) (cdr y) e*)))))))


(defun resolve (x e)
  (cond ((not (pair? x)) x)
        ((var? x)
         (let ((v (value x e)))
           (if (var? v)
               v
             (resolve v e))))
        (t
         (cons
          (resolve (car x) e)
          (resolve (cdr x) e)))))

;;; (define (print-frame e)
;;;   (newline)
;;;   (let loop ((ee e))
;;;     (cond ((pair? (cdr ee))
;;;             (cond ((null? (time (caar ee)))
;;;                     (display (cadaar ee))
;;;                     (display " = ")
;;;                     (display (resolve (caar ee) e))
;;;                     (newline)))
;;;             (loop (cdr ee))))))

(defun print-frame (e)
  (newline)
  (labels ((tail-rec-loop (ee)
             (cond ((pair? (cdr ee))
                    (cond ((null? (htime (caar ee)))
                           (display (cadaar ee))
                           (display " = ")
                           (display (resolve (caar ee) e))
                           (newline)))
                    (tail-rec-loop (cdr ee))))))
    (tail-rec-loop e)))

;;; (defun print-frame (e)
;;;   (newline)
;;;   (loop for ee = e then (cdr ee)
;;;         do
;;;         (cond ((pair? (cdr ee))
;;;                (cond ((null? (htime (caar ee)))
;;;                       (display (cadaar ee))
;;;                       (display " = ")
;;;                       (display (resolve (caar ee) e))
;;;                       (newline))))
;;;               (t (loop-finish)))))

;; Graph example from section 1
(defparameter db1 '(((edge a b))
                   ((edge a f))
                   ((edge a g))
                   ((edge b c))
                   ((edge b d))
                   ((edge c d))
                   ((edge c e))
                   ((edge g h))
                   ((edge d h))
                   ((edge h e))
                   ((edge h f))
                   
                   ((path (:? A) (:? B) ((:? A) (:? B)))
                    (edge (:? A) (:? B)))
                   
                   ((path (:? A) (:? B) ((:? A) . (:? CB)))
                    (edge (:? A) (:? C))
                    (path (:? C) (:? B) (:? CB)))))

(defparameter goals1 '((path a f (:? P))))

;; Negation as failure

(defparameter db2
  '(((some foo))
    ((some bar))
    ((some baz))

    ((eq (:? X) (:? X)))

    ((neq (:? X) (:? Y))
     (eq (:? X) (:? Y)) :! fail)

    ((neq (:? X) (:? Y)))))

(defparameter goals2 '((some (:? X))
                       (some (:? Y))
                       (neq (:? X) (:? Y))))

(defun test2 ()
; 9-slide PROVE
  ;; pt - should result in 6 answers, where X != Y
  (setf *db* db2)
  (prove6 '() goals2 db2 empty 1 '()))

(defparameter goals3 '((some (:? X))
                       (some (:? Y))))

(defun test3 ()
  ;; pt - should result in 9 answers, where sometimes X == Y
  (setf *db* db2)
  (prove6 '() goals3 db2 empty 1 '()))

(defun test4 ()
  (setf *db* db1)
  (prove6 '() goals1 db1 empty 1 '()))


