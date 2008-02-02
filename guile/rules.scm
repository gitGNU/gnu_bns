;;; rules.scm -  Write biological function for rbn (no xor nxor)

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

(load "utilities.scm")

(define (make-index max)
  (if (< max 1001)
      (let ((to9 (map (lambda (i)
			(string-append "00" (number->string i)))
		      (iota 10 0)))
	    (to99 (map (lambda (i)
			 (string-append "0" (number->string i)))
		       (iota 90 10)))
	    (to999 (map number->string (iota 900 100))))
	(take (append to9 to99 to999) max))
      (display "Error in (make-index).")))

(define (create-rule n)
  (let ((f (make-vector n)))
    (let loop ((i 0))
      (cond ((= i n) f)
            (else (vector-set! f i (random 2))
                  (loop (+ 1 i)))))))

(define (write-rule f port)
  (if (output-port? port)
      (map (lambda (x) (display x port)) (vector->list f))
      (error "make-bio-rules:wrong arg" port)))

(define (write-random-rules n n-rules port)
  (let loop ((i 0))
    (if (= i n-rules)
	'()
	(begin
	  (write-rule (create-rule n) port)
	  (newline port)
	  (loop (+ 1 i))))))

(define (write-random-rules/file n n-rules filename)
  (let ((out (open-output-file filename)))
    (write-random-rules n n-rules out)
    (close-output-port out)))


(define (xor? f)
  (if (equal? f '#(0 1 1 0))
      #t
      #f))

(define (notxor? f)
  (if (equal? f '#(1 0 0 1))
      #t
      #f))

(define (all-zeros? f)
  (if (equal? f '#(0 0 0 0))
      #t
      #f))

(define (bio? f)
  (if (or (xor? f) (notxor? f) (all-zeros? f))
      #f
      #t))

(define (make-bio-rules dim n port)
  (let loop ((i 0)
	     (f (create-rule dim)))
    (cond ((= i n) '())
	  ((bio? f) (write-rule f port)
	   (newline port)
	   (loop (+ i 1) (create-rule dim)))
	  (else (loop i (create-rule dim))))))

(define (make-rules-files dim n-rules n-nets prefix)
  (let ((ports (map
                (lambda (x)
		  (open-output-file (string-append prefix x ".rul")))
                (make-index n-nets))))
    (map (lambda (x) (make-bio-rules dim n-rules x)) ports)
    (map close-output-port ports)))
