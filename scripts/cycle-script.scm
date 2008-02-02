#!/usr/bin/guile -s 
!#

(load "../guile/attractors-stats.scm")

(define prefix "/home/user/rbn/attractors/p05/32_2/32_2_")
(define n 100)

(display-cycle-stats prefix 100)
