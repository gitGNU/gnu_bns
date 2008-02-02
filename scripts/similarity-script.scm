#!/usr/bin/guile -s
!#

(load "../guile/similarity.scm")

(define prefix "/home/user/rbn/attractors/p05/32_2/32_2_")
(define n 100)

(similarity-stats-map-run prefix 100)
