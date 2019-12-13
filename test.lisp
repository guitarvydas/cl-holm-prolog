(in-package :hprolog)

(defparameter goal
  `((:mortal (:? cl-user::x))))

(defparameter db
  `(
    ((:man :nils))

    ((:mortal (:? cl-user::x))
     (:man (:? cl-user::x)))

    ))

(defun cl-user::htest-2 ()
  (let ((fb '(((:man nils)))))
    (let ((complete-db fb)
          (initial-db fb)
          (top-link nil)
          (top-env hprolog:*empty*)
          (top-cut nil)
          (top-results-accum nil)
          (top-success :no))
      (hprolog:prove top-link '((:aardvark :nils)) initial-db top-env 1 top-cut top-results-accum complete-db top-success))))

(defun cl-user::htest-1 ()
  (let ((fb '(((:man :nils)))))
    (let ((complete-db fb)
          (initial-db fb)
          (top-link nil)
          (top-env hprolog:*empty*)
          (top-cut nil)
          (top-results-accum nil)
          (top-success :no))
      (hprolog:prove top-link '((:man :nils)) initial-db top-env 1 top-cut top-results-accum complete-db top-success))))

(defun cl-user::htest0 ()
  (let ((complete-db db)
        (initial-db db)
        (top-link nil)
        (top-env hprolog:*empty*)
        (top-cut nil)
        (top-results-accum nil)
        (top-success :no))
    (hprolog:prove top-link '((:man :nils)) initial-db top-env 1 top-cut top-results-accum complete-db top-success)))

(defun cl-user::htest1 ()
  (let ((complete-db db)
        (initial-db db)
        (top-link nil)
        (top-env hprolog:*empty*)
        (top-cut nil)
        (top-results-accum nil)
        (top-success :no))
    (hprolog:prove top-link '((:man (:? cl-user::m))) initial-db top-env 1 top-cut top-results-accum complete-db top-success)))


(defun cl-user::htest2 ()
  (let ((complete-db db)
        (initial-db db)
        (top-link nil)
        (top-env hprolog:*empty*)
        (top-cut nil)
        (top-results-accum nil)
        (top-success :no))
    (hprolog:prove top-link '((:mortal :nils)) initial-db top-env 1 top-cut top-results-accum complete-db top-success)))

(defun cl-user::htest3 ()
  (let ((complete-db db)
        (initial-db db)
        (top-link nil)
        (top-env hprolog:*empty*)
        (top-cut nil)
        (top-results-accum nil)
        (top-success :no))
    (hprolog:prove top-link '((:mortal (:? cl-user::m))) initial-db top-env 1 top-cut top-results-accum complete-db top-success)))



	
#|

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
  (prove '() goals2 db2 empty 1 '() nil))

(defparameter goals3 '((some (:? X))
                       (some (:? Y))))

(defun test3 ()
  ;; pt - should result in 9 answers, where sometimes X == Y
  (setf *db* db2)
  (prove '() goals3 db2 empty 1 '() nil))

(defun test4 ()
  (setf *db* db1)
  (prove '() goals1 db1 empty 1 '() nil))


|#
