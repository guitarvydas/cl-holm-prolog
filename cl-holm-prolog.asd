(defsystem "cl-holm-prolog"
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "hprolog" :depends-on ("package"))))))

(defsystem "cl-holm-prolog/test"
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "testing"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "prolog-6" :depends-on ("package"))))))
