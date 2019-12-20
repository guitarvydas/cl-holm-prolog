(defsystem :cl-holm-prolog
  :depends-on (loops)
  :around-compile (lambda (next)
                    ;(proclaim '(optimize (debug 0) (safety 0) (speed 3)))
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0))) ;; debugging
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "hprolog" :depends-on ("package"))))))

(defsystem :cl-holm-prolog/test
  :depends-on (:cl-holm-prolog)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "test"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "test" :depends-on ("package"))))))
