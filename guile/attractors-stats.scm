;;; attractors-stats.scm - statistics on some attractor's properties

;;; Author: Benelli Marco <mbenelli@yahoo.com>
;;; Date: Juny 2004

;;; Copyright (c) 2004 Benelli Marco

;;; This file is part of BNS.

;;; BNS is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; BNS is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with BNS; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;; This file contains some utilities that work on list of attractors.
;;; See "attractor.scm" for more information about attractors.

(load "utilities.scm")
(load "attractor.scm")
(load "stats.scm")

;;; Return attribute specified by ACCESSOR from each element of ATTRACTOR-LIST.
(define (get-list accessor attractor-list)
  (cond
   ((not (procedure? accessor)) (error "get-list, wrong arg:" accessor))
   ((not (list? attractor-list)) (error "get-list, wrong arg:" attractor-list))
   (else (map accessor attractor-list))))

;;; Return (mean std-dev) for attribute specified by ACCESSOR.
(define (get-average accessor attractor-list)
  (m_sd (delete '() (get-list accessor attractor-list))))

;;; Return number of tests that haven't reached an attracor.
;;; !!!! It doesn't work.
(define (not-reached attractor-list n)
  (if (null? attractor-list)
      '()
      (- n 
	 (* (length attractor-list) 
	    (car (get-average attractor-basin attractor-list))))))

;;; Number of attractors in ATTRACTOR-LIST with period 1.
(define (fixed-points attractor-list)
  (let 
      ((fixed-point?
	(lambda (x)
	  (if (= 1 (attractor-size x))
	      #t
	      #f))))
    (count fixed-point? (delete '() attractor-list))))

;;; Average size, number of fixed points, average basin size,
;;; average number of floating nodes.
(define (attractors-stats attractor-list)
  (let ((n (length attractor-list))
	(size (get-average attractor-size attractor-list))
	(fixed-pts (fixed-points attractor-list))
	(basin (get-average attractor-basin attractor-list))
	(floating (get-average attractor-n-floating attractor-list)))
    (list n (car size) fixed-pts (car basin) (car floating))))

;;; A reduced version of attractors-stats.
;;; Return only number of attractor, average size
;;; and average number of floating point.
(define (simple-stat attractor-list)
  (let ((n (length attractor-list))
	(size (get-average attractor-size attractor-list))
	(floating (get-average attractor-n-floating attractor-list)))
    (list n size floating)))

;;; Find min, max, mean and median and put they in a list.
;;; !!! MIN and MAX must be reimplemented.
(define (item-stats lst)
  (let ((min-value (min lst))
	(max-value (max lst))
	(mean-value (mean lst))
	(median-value (median lst)))
    (list min-value max-value mean-value median-value)))

;;; Apply ITEM-STATS on a sample.
;;; LST is: ((n_0 size_0 floating_0) (n_1 size_1 floating_1) ...)
;;; as returned by SIMPLE-STAT.
(define (sample-stats lst)
  (let ((n (item-stats (map car lst)))
	(size (item-stats (map caadr lst)))
	(floating (item-stats (map caaddr lst))))
    (list n size float)))

;;; Print a list.
;;; !!! Why is it here ???!!!
(define (print-list lst)
  (map (lambda (x) (simple-format #t "~a\t" x)) lst))

;;; Run F on N attractor readed from files in PREFIX.
(define (run-with-prefix prefix f n)
  (let ((index (make-3dgts-index n)))
    (let loop ((i index))
      (if (null? i)
	  (display "evaluation complete.\n")
	  (let ((id (string-append prefix (car i) ".scm")))
	    (load id)
	    (display (car i))
	    (display #\tab)
	    (f (read-attractor-list cyclic) (read-attractor-list loose))
	    (newline)
	    (loop (cdr i)))))))


;;; Apply some functions on sets of attractors.
(define (display-cycle-size prefix n)
  (run-with-prefix prefix
		   (lambda (x y)
		     (print-list (get-list attractor-size x)))
		   n))

(define (display-loose-size prefix n)
  (run-with-prefix prefix
		   (lambda (x y)
		     (print-list (get-list attractor-size y)))
		   n))

(define (display-cycle-basins prefix n)
  (run-with-prefix prefix
		   (lambda (x y)
		     (print-list (get-list attractor-basin x)))
		   n))

(define (display-loose-basins prefix n)
  (run-with-prefix prefix
		   (lambda (x y)
		     (print-list (get-list attractor-basin y)))
		   n))

(define (display-cycle-stats prefix n)
  (display "Id\tN\tSize\tPA\tBasin\tFloating\n")
  (run-with-prefix prefix (lambda (x y) (print-list (attractors-stats x))) n))

(define (display-loose-stats prefix n)
  (display "Id\tN\tSize\tPA\tBasin\tFloating\n")
  (run-with-prefix prefix (lambda (x y)  (print-list (attractors-stats y))) n))
