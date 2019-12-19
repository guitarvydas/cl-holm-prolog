This code is buggy, but causes an unexpected register dump while debugging a new feature that I'm trying to suss out.  
(See (:lisp) in test.lisp - the problem is related to that line.  If I "#+nil" the line, everything works fine).
The cl-holm-prolog code was ported from Scheme (r5rs) http://t3x.org/bits/prolog6.html.  It worked in its virgin form, until I started to add this feature.

(ql:quickload :cl-holm/prolog/test)
(cl-holm-prolog::ltest)
<segfault>

This sequence also causes a memory corruption message in SBCL.

