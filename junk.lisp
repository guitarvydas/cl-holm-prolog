    #+nil(let ((lisp-clause (car g)))
      (format *standard-output* "~%apply ~S ~S~%" (second lisp-clause) (append (list self l (cdr g) r e n c result) (rest (rest lisp-clause))))
      (multiple-value-bind (new-l new-g new-r new-e new-n new-c new-result)
          (apply (second lisp-clause) (append (list self l (cdr g) r e n c result)
                                              (rest (rest lisp-clause))))
        (prove new-l new-g new-r new-e new-n new-c complete-db new-result self)))

(:lisp (lambda (l g r e n c result)
                         (format *standard-output* "~&found ellipse ~A ~A ~A ~A ~A~%"
                                 (resolve 'id e) (resolve 'cx e) (resolve 'cy e)
                                 (resolve 'hw e) (resolve 'hh e))
                         (values (l g r e n c result))))b
