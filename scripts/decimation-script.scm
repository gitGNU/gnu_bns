#!/usr/bin/guile -s
!#

(load "../guile/decimate.scm")

(define in "/home/user/rbn/nets/32_2/32_2_0")
(define out "../tests/32_2_")
(define n 100)
(define init "/home/user/rbn/nets/init_32_100_1.ini")

(decimate-run-with-inits in out n init)
