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

(defun back (l g r e n c complete-db result self)
  (cond
   ((and (pair? g)
         (pair? r))
    (prove l g (cdr r) e n c complete-db result self))
   ((pair? l)
    (prove (L_l l) (L_g l) (cdr (L_r l)) (L_e l) (L_n l) (L_c l) complete-db result self))))

(defun prove (l g r e n c complete-db result self)
  (cond
   ((null? g)
    (back l g r e n c complete-db (cons (collect-frame e) result) self))
   ((eq? :! (car g))
    (clear_r c)
    (prove c (cdr g) r e n c complete-db result self))
   ((eq? :r! (car g))
    (prove l (cddr g) r e n (cadr g) complete-db result self))

   ((and (listp (car g))
         (eq :lisp (caar g)))
    (let ((lisp-colon-clause (first g))) ; (:lisp (fn arg arg ...))
      (let ((sexpr (second lisp-colon-clause)))
        (let ((fn (first sexpr))
              (arglist (mapcar #'(lambda (lis)
                                   (mapcar #'(lambda (x)
                                               (if (and (listp x) (eq :? (car x)))
                                                   (resolve x e)
                                                 x))
                                           lis))
                               (rest sexpr))))
          (multiple-value-bind (ll gg rr ee nn cc resultresult) 
              (apply fn (append arglist (list l g r e n c result))) (declare (ignore gg))
            (prove ll (cdr g) rr ee nn cc complete-db resultresult self))))))

   ((null? r)
    (if (null? l)
        result
      (back l g r e n c complete-db result self)))
   (t
    (let* ((a  (copy (car r) n)))
      (multiple-value-bind (e* success)
          (unify (car a) (car g) e)
        (if success
            (prove (link l g r e n c)
                   (append (cdr a) `(:r! ,l) (cdr g))
                   complete-db ;; ! - start from top
                   e*
                   (+ 1 n)
                   l
                   complete-db
                   result
                   self)
          (back l g r e n c complete-db result self)))))))


(defparameter *empty* '((:bottom)))

(defun name (x) (cadr x))
(defun htime (x) (cddr x))

(defun var? (x)
  (and (pair? x)
       (eq? :? (car x))))

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
  ;; return (values bindings success)
  (let ((x (value x e))
        (y (value y e)))
    (cond
     ((eq? x y) (values e t))
     ((var? x) (values (bind x y e) t))
     ((var? y) (values (bind y x e) t))
     ((or (not (pair? x))
          (not (pair? y))) (values +false+ nil))
     (t
      (multiple-value-bind (e* success)
          (unify (car x) (car y) e)
        #+nil(format *standard-output* "~&unify e*/success ~S ~S~%" e* success)
        (if success
            (multiple-value-bind (ee* success2)
                (unify (cdr x) (cdr y) e*)
              #+nil(format *standard-output* "~&unify/2 ee*/success2 ~S ~S~%" ee* success2)
              (if success2
                  (progn
                    #+nil(format *standard-output* "~&unify returns T ee*=~S~%" ee*)
                    (values ee* t))
                (progn
                    #+nil(format *standard-output* "~&unify returns #f #f~%" ee*)
                    (values +false+ nil))
                ))
          (progn
            #+nil(format *standard-output* "~&unify returns #f #f~%" ee*)
            (values +false+ nil))))))))

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

(defun collect-frame (e)
  (let ((result nil))
    (labels ((tail-rec-loop (ee)
               (cond ((pair? (cdr ee))
                      (cond ((null? (htime (caar ee)))
                             (push (cons (cadaar ee) (resolve (caar ee) e)) result)                            
                             (tail-rec-loop (cdr ee))))))))
      (tail-rec-loop e))
    result))
