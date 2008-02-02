;;; stats.scm - some basic statistical functions

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


;;; Type check.
(define (list-of-num? lst)
  (cond
    ((null? lst) #f)
    ((list? lst) 
     (let loop ((elts lst))
                   (cond 
                     ((null? elts) #t)
                     ((number? (car elts)) (loop (cdr elts)))
                     (else #f))))
    (else #f)))
                      
;;; Mean.
(define (mean lst)
  (if (list-of-num? lst)
      (/ (apply + lst) (length lst))
      (error "wrong type")))

;;; Standard deviation.
(define (stddev lst)
  (if (list-of-num? lst)
      (let ((m (mean lst))
            (n (length lst)))
        (let loop ((elts lst)
                   (sum 0)
                   (sum2 0))
          (if (null? elts)
              (sqrt (/ (- (/ sum2 n) (* m m)) (- n 1)))
              (loop (cdr elts)
                    (+ sum (car elts))
                    (+ sum2 (* (car elts) (car elts)))))))                     
      (error "wrong type")))

;;; Mean and standard deviation (from mean).
(define (m_sd lst)
  (cond  ; (list-of-num? lst)
   ((null? lst) (list '() '()))
   ((list-of-num? lst)
    (if (= 1 (length lst))
	(list (car lst) 0)
	(let loop ((elts lst)
		   (n 0)
		   (sum 0)
		   (sum2 0))
	  (if (null? elts)
	      (let ((m (/ sum n)))
		(list (exact->inexact m) 
		      (exact->inexact (sqrt (/ (- (/ sum2 n) (* m m))
					       (- n 1))))))
	      (let ((x (car elts))
		    (rest (cdr elts)))
		(loop rest
		      (+ 1 n)
		      (+ sum x)
		      (+ sum2 (* x x))))))))            
   (else (error "m_sd, wrong type:" lst))))


(define (percentile sequence percent)
  (let* ((sorted-list (sort sequence <))
         (n (length sorted-list))
         (k (* n (/ percent 100)))
         (floor-k (inexact->exact (floor k))))
    (if (= k floor-k)
        (/ (+ (list-ref sorted-list k)
              (list-ref sorted-list (- 1 k)))
           2)
        (list-ref sorted-list floor-k))))

;;; Median
(define (median lst)
	(percentile lst 50))

	
