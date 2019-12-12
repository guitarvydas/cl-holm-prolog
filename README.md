This is a CL implemention of Nils M Holm's "Prolog Control in Six Slides" http://www.t3x.org/bits/prolog6.html .

A logic variable is (:? a).

A rule is a head, followed by a body:

((head (:? a) (:?b))
       (body (:? a))
       (body (:? b)))

A fact is a single listoid:

((fact x y))

A factbase is a list of rules + facts:

(((head (:? a) (:?b))
 	(body (:? a))
	(body (:? b)))
((fact x y)))
