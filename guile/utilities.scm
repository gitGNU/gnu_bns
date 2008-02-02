;;; utilities.scm - Ssome useful utilities

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

(use-modules (srfi srfi-1))

;;; Return TRUE if  all elements of lst satisfy PREDICATE
(define (andmap predicate lst . lst0)
  (cond ((null? lst0)
	 (if (list? lst)
	     (let ((len (length lst)))
	       (let loop ((elts lst)
			  (counter 0))
		 (cond ((= counter len) #t)
		       ((predicate (car elts)) (loop (cdr elts) (+ 1 counter)))
		       (else #f))))))
	((= (length lst) (length (car lst0)))
	 (let loop ((l1 lst)
		    (l2 (car lst0)))
	   (cond ((null? l1) #t)
		 ((predicate (car l1) (car l2))
		  (loop (cdr l1) (cdr l2)))
		 (else #f))))
	(else (error "andmap: for now it manges only 1 or 2 lists."))))

;;; Return the number of digits of N (must be integer).
(define (%n-digits n)
  (if (integer? n)
      (if (= n 0)
	  1
	  (let loop ((digits 0))
	    (if (> 1 (/ n (expt 10 digits)))
		digits
		(loop (+ 1 digits)))))
      (error "%n-digits? : wrong arg")))

;;; Return a string representing N with FILL digits.
;;; Example: (%fill-with-zeroes 3 5) --> 00003
(define (%fill-with-zeroes n fill)
  (if (and (integer? n)
	   (integer? fill))
      (let ((needed (- fill (%n-digits n)))
	    (result (number->string n)))
	(let loop ((i needed)
		   (string result))
	  (if (= 0 i)
	      string
	      (loop (- i 1) (string-append "0" string)))))
      (error "%fill-with-zeroes" : wrong arg)))

;;; Return a list of strings representing integers from 0 to MAX.
(define (make-index max)
  (if (integer? max)
      (let* ((elts (iota max))
	     (dgts (%n-digits (car (reverse elts)))))
	(map (lambda (x) (%fill-with-zeroes x dgts)) elts))
      (error "make-index")))
