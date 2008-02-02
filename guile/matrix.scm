;;; matrix.scm - rapresentation of 2-dimensional arrays

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


;;; Matrix. (2-dim arrays)
;;; Defined for print results of SIMILARITY in symmetric matrix.

;;; Sum integer from 1 to N.
(define (sum-to-n n)
  (/ (* n 
	(+ n 1)) 
     2))

;;; Return a number X so that: (= N (sum-to-n X))
(define (sum-to-n-inverse n)
  (/ (- (sqrt (+ 1 
		 (* 8 n)))
	1)
     2))

;;; Check if LST can represent a simmetric matrix.
(define (simmetric? lst)
  (if (integer? (sum-to-n-inverse (length lst)))
      #t
      #f))

;;; Substitute 'inc> with 'inc< or 'inc< with 'inc>.
(define (reverse-inclusion v)
  (if (list? v)
      (cond
       ((eq? (car v) 'inc>) (cons 'inc< (cdr v)))
       ((eq? (car v) 'inc<) (cons 'inc> (cdr v)))
       (else v))      
      (cond
       ((eqv? v 'inc>) 'inc<)
       ((eqv? v 'inc<) 'inc>)
       (else v))))

;;; Build a "simmetric" matrix from a list (e[0] e[1] ... e[k] ...).
;;; Diagonal elments aren't initialized. The form is:
;;; <>     e[0]   e[1] e[2]   ...
;;; e[0]   <>     e[k] e[k+1] ...
;;; e[1]   e[k]   <>   ...    ...
;;; e[2]   e[k+1] ...  ...    ...
;;; ...    ...    ...  ...    <>
;;; This is really an antisimmetric matric because inclusions are reversed.
(define (list->simmetric-matrix lst)
  (let ((max-len (sum-to-n-inverse (length lst))))
    (if (integer? max-len)
	(let* ((rank (+ 1 (inexact->exact max-len)))
	       (matrix (make-array 0 (list 0 rank) (list 0 rank))))
	  (let loop ((i 0)
		     (j 0)
		     (values lst))
	    (cond
	     ((and (= i max-len) (null? values))
	      matrix)
	     ((= j rank)
	      (loop (+ 1 i) 0 values))
	     ((or (> i j) (= i j))
	      (loop i (+ 1 j) values))
	     (else
	      (array-set! matrix (car values) i j)
	      (array-set! matrix (reverse-inclusion (car values)) j i)
	      (loop i (+ 1 j) (cdr values))))))
	(display "Impossible create simmetric matrix from list\n"))))

;;; Unsafe functions: there aren't argument cheks.

;;; Fill MATRIX's diagonal elements with VALUE.
(define (%fill-diagonal matrix value)
  (let ((rank (- (array-end matrix 0) (array-start matrix 0))))
    (let loop ((i 0))
      (if (= i rank)
	  matrix
	  (begin
	    (array-set! matrix value i i)
	    (loop (+ 1 i)))))))

;;; Build labels for rows and columns ("c0" "c1" ... "cclen" "l0"... "lllen").
(define (make-labels clen llen)
  (let ((cnum (iota clen))
	(lnum (iota llen))
	(cid (lambda (x) (string-append "c" (number->string x))))
	(lid (lambda (x) (string-append "l" (number->string x)))))
    (append (map cid cnum) (map lid lnum))))

(define (array-start a i)
  (car (list-ref (array-shape a) i)))

(define (array-end a i)
  (cadr (list-ref (array-shape a) i)))

(define (display-matrix a)
  (if (= 2 (array-rank a))
      (let loop1 ((i (array-start a 0)))
	(if (= i (array-end a 0))
	    (newline)
	    (begin
	      (let loop2 ((j (array-start a 1)))
		(if (= j (array-end a 1))
		    '() 
		    (begin
		      (display (array-ref a i j))
		      (display "\t")
		      (loop2 (+ 1 j)))))
	      (display #\newline)
	      (loop1 (+ 1 i)))))
      (display "Not a matrix")))
