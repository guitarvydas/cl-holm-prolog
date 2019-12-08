(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (loop for i in letargs
                  collect (gensym))))
    `(macrolet
       ((,n ,gs
          `(progn
             (psetq
               ,@(apply #'nconc
                        (mapcar
                          #'list
                          ',(mapcar #'car letargs)
                          (list ,@gs))))
             (go ,',g!n))))
       (block ,g!b
         (let ,letargs
           (tagbody
             ,g!n (return-from
                    ,g!b (progn ,@body))))))))

(defun nlet-tail-fact (n)
  (nlet-tail fact ((n n) (acc 1))
    (if (zerop n)
      acc
      (fact (- n 1) (* acc n)))))
