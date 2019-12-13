(in-package :cl-holm-prolog)

(defconstant +true+ t)
(defconstant +false+ nil)

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

(defun show8 (rule l g r e n c results-accumulator complete-db success)
  (declare (ignorable complete-db l g r e n c results-accumulator))
  (format *standard-output* "~&~S ~S ~S ~S ~S ~S ~S ~S ~S~%~%" rule l g r e n c results-accumulator success))

(defun back (l g r e n c results-accumulator complete-db success)
  (show8 :back l g r e n c results-accumulator complete-db success)
  (cond
   ((and (pair? g)
         (pair? r))
    (prove l g (cdr r) e n c results-accumulator complete-db success))
   ((pair? l)
    (prove (L_l l) (L_g l) (cdr (L_r l)) (L_e l) (L_n l) (L_c l) results-accumulator complete-db success))))

(defun prove (l g r e n c results-accumulator complete-db success)
  (show8 :prove l g r e n c results-accumulator complete-db success)
  (cond
   ((null? g)
    (back l g r e n c (cons (collect-frame e) results-accumulator) complete-db success))
   ((eq? :! (car g))
    (clear_r c)
    (prove c (cdr g) r e n c results-accumulator complete-db success))
   ((eq? :r! (car g))
    (prove l (cddr g) r e n (cadr g) results-accumulator complete-db success))
   ((null? r)
    (if (null? l)
        results-accumulator
      (back l g r e n c results-accumulator complete-db :yes)))
   (t
    (let* ((a  (copy (car r) n))
           (e* (unify (car a) (car g) e)))
      (if e*
          (prove (link l g r e n c)
                  (append (cdr a) `(:r! ,l) (cdr g))
                  complete-db
                  e*
                  (+ 1 n)
                  l
                  results-accumulator
                  complete-db
                  :yes)
        (back l g r e n c results-accumulator complete-db success))))))


(defparameter *empty* '((:bottom)))

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

(defun collect-frame (e)
  (let ((result nil))
    (labels ((tail-rec-loop (ee)
               (cond ((pair? (cdr ee))
                      (cond ((null? (htime (caar ee)))
                             (push
                              (cons (cadaar ee) (resolve (caar ee) e))
                              result))
                            (t (tail-rec-loop (cdr ee)))))
                     (t 'yes))))
      (tail-rec-loop e)
      result)))
    
