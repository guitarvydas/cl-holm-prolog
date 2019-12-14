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
          (top-cut nil))
      (hprolog:prove top-link '((:aardvark :nils)) initial-db top-env 1 top-cut complete-db))))

(defun cl-user::htest-1 ()
  (let ((fb '(((:man :nils)))))
    (let ((complete-db fb)
          (initial-db fb)
          (top-link nil)
          (top-env hprolog:*empty*)
          (top-cut nil))
      (hprolog:prove top-link '((:man :nils)) initial-db top-env 1 top-cut complete-db))))

(defun cl-user::htest0 ()
  (let ((complete-db db)
        (initial-db db)
        (top-link nil)
        (top-env hprolog:*empty*)
        (top-cut nil))
    (hprolog:prove top-link '((:man :nils)) initial-db top-env 1 top-cut complete-db)))

(defun cl-user::htest1 ()
  (let ((complete-db db)
        (initial-db db)
        (top-link nil)
        (top-env hprolog:*empty*)
        (top-cut nil))
    (hprolog:prove top-link '((:man (:? cl-user::m))) initial-db top-env 1 top-cut complete-db)))


(defun cl-user::htest2 ()
  (let ((complete-db db)
        (initial-db db)
        (top-link nil)
        (top-env hprolog:*empty*)
        (top-cut nil)
        (top-results-accum nil)
        (top-success :no))
    (hprolog:prove top-link '((:mortal :nils)) initial-db top-env 1 top-cut complete-db)))

(defun cl-user::htest3 ()
  (let ((complete-db db)
        (initial-db db)
        (top-link nil)
        (top-env hprolog:*empty*)
        (top-cut nil))
    (hprolog:prove top-link '((:mortal (:? cl-user::m))) initial-db top-env 1 top-cut complete-db)))
