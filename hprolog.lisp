(in-package :cl-holm-prolog)

(defparameter *?* :?)

(defparameter *trace* nil)

(defconstant +true+ t)
(defconstant +false+ nil)

(defun null? (x) (null x))
(defun pair? (x) (and (not (null x)) (or (listp x) (consp x))))
(defun eq? (x y) (equal x y))
(defun eqv? (x y) (eql x y))

(defun newline () (format *standard-output* "~%"))

(defun display (x) (format *standard-output* "~A" x))

#|
(defmacro link (&rest x) `(list ,@x))
(defmacro L_l (x) `(car ,x))
(defmacro L_g (x) `(cadr ,x))
(defmacro L_r (x) `(caddr ,x))
(defmacro L_e (x) `(cadddr ,x))
(defmacro L_n (x) `(car (cddddr ,x)))

(defun L_c (x) (cadr (cddddr x)))
|#

(defmacro link (&rest x) `(list ,@x))
(defmacro L_l (x) `(first ,x))
(defmacro L_g (x) `(second ,x))
(defmacro L_r (x) `(third ,x))
(defmacro L_e (x) `(fourth ,x))
(defmacro L_n (x) `(fifth ,x))
(defmacro L_c (x) `(sixth ,x))
(defmacro L_depth (x) `(seventh ,x))


(defun clear_r (x)
  (rplaca (cddr x) '(())))
  ;(set-car! (cddr x) '(())))

(defun back (l g r e n c depth complete-db result self)
  (cond
   ((and (pair? g)
         (pair? r))
    (prove-helper l g (cdr r) e n c depth complete-db result self))
   ((pair? l)
    (prove-helper (L_l l) (L_g l) (cdr (L_r l)) (L_e l) (L_n l) (L_c l) (L_depth l) complete-db result self))))

(defun prove (l g r e n c complete-db result self)
  (let ((r (prove-helper l g r e n c 0 complete-db result self)))
    (unless (null r)
      (substitute :yes nil r))))
        

(defun expand-vars (args e)
  (mapcar #'(lambda (y)
              (if (and (listp y) (member (car y) '(asserta retract)))
                  (mapcar #'(lambda (x)
                              (if (and (listp x) (eq *?* (car x)))
                                  (resolve x e)
                                x))
                          y)
                (resolve y e)))
          args))


;; a db is ( (...) (...) ... ), 
;; each list in the db is a rule or a fact
;; a rule is ((a) (b) (c)) where (a) is the head, ((b) (c)) is the body of the rule
;; a fact is ((fact))
;; a goal is ( (e) ...) where (e) is a single relation to be matched
;; a logic variable is (? var) 
(defun prove-helper (l g r e n c depth complete-db result self)
  (cond
   ((null? g)
    (when (trace-p)
      (if g
	  (tab-in *standard-output* depth)
          (format *standard-output* "prove SUCCESS~%" (car g))))
    (back l g r e n c (dec-depth depth) complete-db (cons (collect-frame e) result) self))
   ((eq? :! (car g))
    (clear_r c)
    (prove-helper c (cdr g) r e n c depth complete-db result self))
   ((eq? :r! (car g))
    (prove-helper l (cddr g) r e n (cadr g) depth complete-db result self))

   ((eq :rule (car g))
    (prove-helper l (cdr g) r e n c depth complete-db result self))

   ((and (listp r) (listp (car r)) (or (eq :rule (caar r))
                                       (eq :fact (caar r))))
    ;; r = ((:rule ...) ...) --> ((...)...)
    (prove-helper l g (cons (cdar r) (cdr r)) e n c depth complete-db result self))

   ((and (listp (car g))
         (eq :lispv (caar g)))
    (let ((lispv-clause (first g))) ; (:lispv (? xx) (fn arg arg ...)) ... ) xx is bound to result of call (fn self arg arg ...), unless xx is _
      (assert (= 3 (length lispv-clause))) ;; the :lispv form is badly formed if this assert fails
      (let ((var-clause (second lispv-clause))
            (sexpr (third lispv-clause)))
        (if (symbolp var-clause)
            (assert (string= "_" var-clause))
          (when (var-in-environment-p var-clause e)
            (error (format nil "~&cannot use same variable ~S, since it is already in the environment ~S~%" var-clause e))))
        (let ((var (if (and (symbolp var-clause) (string= "_" (symbol-name var-clause)))
                       :dont-care
                     (second var-clause)))
              (fn (first sexpr))
              (arglist (expand-vars (rest sexpr) e)))
          (let ((lispv-r (apply fn arglist)))
            (let ((e* (if (eq :dont-care var)
                          e
                        (cons (list var-clause lispv-r) e))))
              (prove-helper l (cdr g) r e* n  c depth complete-db result self)))))))

   ((and (listp (car g))
         (eq :lisp (caar g))) ;; call LISP, always succeed, args are NOT eval'ed (e.g. (:lisp (format *standard-output* ...)) does not work)
;(format *standard-output* "~&(car g) ~S e=~S~%" (car g) e)  
    (let ((lisp-colon-clause (first g))) ; (:lisp (fn arg arg ...))
;(format *standard-output* "~&lisp-colon-clause ~S~%" lisp-colon-clause)          
      (assert (= 2 (length lisp-colon-clause))) ;; the :lisp form is badly formed if this assert fails
      (let ((sexpr (second lisp-colon-clause)))
        (let ((fn (first sexpr))
              (arglist (expand-vars (rest sexpr) e)))
;(format *standard-output* "~&sexpr=~S fn=~S arglist=~S~%" sexpr fn arglist)          
          (apply fn arglist)
          (prove-helper l (cdr g) r e n c depth complete-db result self)))))

   ((and (listp (car g))
         (eq :lisp-true-fail (caar g))) ;; call LISP, lisp returns NIL or T which maps to :fail and :true paths
    (let ((lisp-colon-clause (first g))) ; (:lisp (fn arg arg ...))
      (assert (= 2 (length lisp-colon-clause))) ;; the :lisp form is badly formed if this assert fails
      (let ((sexpr (second lisp-colon-clause)))
        (let ((fn (first sexpr))
              (arglist (expand-vars (rest sexpr) e)))
          (let ((val (apply fn arglist)))
            (if val
                (prove-helper l (cdr g) r e n c depth complete-db result self)
              (back l g r e n c depth complete-db result self)))))))
      

   ((and (listp (car g))
         (eq :lisp-method (caar g))) ;; call a LISP METHOD with SELF, success depends on method return value
    (let ((lisp-colon-clause (first g))) ; (:lisp (fn arg arg ...))
      (assert (= 2 (length lisp-colon-clause))) ;; the :lisp form is badly formed if this assert fails
      (let ((sexpr (second lisp-colon-clause)))
        (let ((fn (first sexpr))
              (arglist (expand-vars (rest sexpr) e)))
          (multiple-value-bind (success ll gg rr ee nn cc resultresult) 
              (apply fn (append (cons self arglist) (list l g r e n c result)))
            (declare (ignore gg))
            (if success
                (prove-helper ll (cdr g) rr ee nn cc depth complete-db resultresult self)
              (back l g r e n c depth complete-db result self)))))))

   ((and (listp (car g))    ;; g = ((op x y z) ...)
         (not (numberp (htime g)))
         (string-member '("NOT" ">=" "<=" ">" "<") (symbol-name (caar g))))
    (let ((lisp (car g)))
      (let ((op (intern (symbol-name (first lisp)) "CL"))
            (args (expand-vars (rest lisp) e)))
        (let ((r (apply op args)))
          (if r
              (prove-helper l (cdr g) r e n c depth complete-db result self)
            (back l g r e n c depth complete-db result self))))))

   ((and (listp (car g))    ;; g = (true ...)
         (string= "TRUE" (symbol-name (caar g))))
    (prove-helper l g r e n c depth complete-db result self))
   
   ((and (listp (car g))    ;; g = (false ...)
         (string= "FAIL" (symbol-name (caar g))))
    (back l g r e n c depth complete-db result self))

   ((and (listp (car g))
         (eq :trace-on (caar g)))
    (let ((level (if (numberp (second (first g)))
                     (second (first g))
                   1)))
      (setf *trace* level)
      (format *standard-output* "~&TRACE ~A~%" *trace*)
      (prove-helper l (cdr g) r e n c depth complete-db result self)))

   ((and (listp (car g))
         (eq :trace-off (caar g)))
    (setf *trace* nil)
    (format *standard-output* "~&TRACE ~A~%" *trace*) 
    (prove-helper l (cdr g) r e n c depth complete-db result self))

   ((null? r)
    (if (null? l)
        result
      (back l g r e n c depth complete-db result self)))
   (t
    (let* ((a  (copy (car r) n))) ;; creates unique variables for (car r)
      (multiple-value-bind (e* success)
          (unify (car a) (car g) e)
        (if success
            (progn
              (when (trace-p)
		(tab-in *standard-output* depth)
                (format *standard-output* "Unified ~S ~S~%" (car a) (car g)))
              (let ((next-goal (append (cdr a) `(:r! ,l) (cdr g)))) ;; g gets [(cdr r') (r! ,l) (cdr g)] where (cdr r') is a copy of the body of a rule
                (when (trace-verbose-p)
		  (tab-in *standard-output* depth)
                  (format *standard-output* "next goal ~S~%" (car next-goal)))
                (let ((calling-subr-p (not (null (cdr a)))))
                  (prove-helper (link l g r e n c depth)
                                next-goal
                                complete-db ;; ! - start from top
                                e*
                                (+ 1 n)
                                l
                                (if calling-subr-p
                                    (1+ depth)
                                  depth)
                                complete-db
                                result
                                self))))
              (progn
                (when (trace-verbose-p)
                  (format *standard-output* "."))
                (when (trace-failure-p)
                  (tab-in *standard-output* depth)
                  (format *standard-output* "failed to unify /~S/ /~S/~%" (car a) (car g)))
                (back l g r e n c depth complete-db result self))))))))

(defun trace-failure-p ()
  (and (numberp *trace*)
       (> *trace* 2)))

(defun trace-verbose-p ()
  (and (numberp *trace*)
       (> *trace* 1)))

(defun trace-p ()
  (and (numberp *trace*)
       (> *trace* 0)))


(defparameter *empty* '((:bottom)))

(defun name (x) (cadr x))
(defun htime (x) (cddr x))

(defun var? (x)
  (and (pair? x)
       (eq? *?* (car x))))

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

  #+nil (unless (eq x t)
    (when (and x
               (symbolp x)
               (not (eq x *empty*))
               (not (eq (find-package "KEYWORD") (symbol-package x))))
      (error (format nil "goals must use KEYWORD symbols, but got ~S" x))))
    
  #+nil(unless (eq y t)
    (when (and y
               (symbolp y)
               (not (eq y *empty*))
               (not (eq (find-package "KEYWORD") (symbol-package y))))
      (error (format nil "goals must use KEYWORD symbols, but got ~S" y))))

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
                             (push (cons (cadaar ee) (resolve (caar ee) e)) result)))                         
                      (tail-rec-loop (cdr ee))))))
      (tail-rec-loop e))
    result))

(defun string-member (string-list str)
  (mapc #'(lambda (x)
            (when (string= x str)
              (return-from string-member t)))
        string-list)
  nil)

(defun var-in-environment-p (var env)
  (assert (listp env))
  (assert (var? var))
  (if (null env)
      nil
    (let ((first-var (caar env)))
      (when (listp first-var)
        (when (var? first-var)
          (when (and (eq (name first-var) (name var))
                     (eq (htime first-var) (htime var)))
            (return-from var-in-environment-p t))))
      (var-in-environment-p var (cdr env)))))
            

(defun tab-in (stream n)
  (format stream "~&")
  (@:loop
    (@:exit-when (<= n 0))
    (format stream " ")
    (decf n)))

(defun dec-depth (n)
  ;; might use (min 0 (1- n)) in the future, after debugging
  (1- n))


;; replace variables in lis with actual values given by alis
(defun reify (lis alis)
  (cond ((null lis) nil)
        ((not (listp lis)) lis)
        ((listp lis)
         (let ((looks-like-a-var (and (= 2 (length lis)) (eq ':? (first lis)))))
           (cond (looks-like-a-var
                  (let ((sym (second lis)))         
                    (let ((vl (assoc sym alis)))
                      (cond (vl (cdr vl))
                            ((null vl) lis)))))
                 ((not looks-like-a-var)
                  (cons (reify (car lis) alis)
                         (reify (cdr lis) alis))))))))
