;;; similarity.scm - relation of similarity between cyles (synchronous) and loose (asynchronous)

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

(use-modules (ice-9 format))

(load "attractor.scm")
(load "attractors-stats.scm")
(load "matrix.scm")

;;; Evaluate some parameters about "similarity" of different attractors.
;;; Given two attractors A and B:
;;;
;;; m = i/u where: i = number of nodes that are floating in A and B;
;;;                u = number of nodes that are floating in A or B;
;;; m2 = i2/u2 where: i2 = number of nodes that are fixed in A and B;
;;;                   u2 = number of nodes that are fixed in A or B;
;;; m3 = i3/u3 where: i3 = number of nodes that are fixed at the same
;;;                        value in A and B;
;;;                   u3 = u2;
;;; d = number of different nodes / number of nodes
;;; [different in symbols (0,1,*) where * means "floating".]
;;;

(define (%similarity a b)
  (if (andmap attractor? (list a b))
      (let* ((i (length (floating-intersection a b)))
	     (u (length (floating-union a b)))
	     (i2 (length (outer-core a b)))
	     (u2 (length (fixed-union a b)))
	     (i3 (length (inner-core a b)))
	     (u3 u2)
	     (n (vector-length (attractor-id a))) 
	     (dismatch (- n (+ i i3)))
	     (numbers->ratio-str (lambda (x y)
				   (string-append (number->string x) 
						  "/" 
						  (number->string y)))))
	(list
	 (numbers->ratio-str i u)
	 (numbers->ratio-str i2 u2)
	 (numbers->ratio-str i3 u3)
	 (numbers->ratio-str dismatch n)))
      (error "%similarity, wrong args:" a b)))

;;; Inclusion : nodes that are fixed in A and B have the same value in A and B.
;;; Floating nodes are in the same position.
(define (inclusion a b)
  (if (andmap attractor? (list a b))
      (let ((a-fixed (attractor-fixed a))
	    (b-fixed (attractor-fixed b))
	    (a-floating (attractor-floating a))
	    (b-floating (attractor-floating b)))
	(cond
	 ((not (null? (different-fixed a b))) (%similarity a b))
	 ((and (lset<= eq? a-fixed b-fixed)
	       (lset<= eq? b-floating a-floating)) 'inc>)
	 ((and (lset<= eq? b-fixed a-fixed)
	       (lset<= eq? a-floating b-floating)) 'inc<)
	 (else (%similarity a b))))
      (error "inclusion, wrong args" a b)))

;;; Alternative inclusion: fixed nodes in common may have different values.
;;; Return inclusion simbol and number of different values in fixed nodes.
(define (weak-inclusion a b)
  (if (andmap attractor? (list a b))
      (let ((a-fixed (attractor-fixed a))
	    (b-fixed (attractor-fixed b))
	    (a-floating (attractor-floating a))
	    (b-floating (attractor-floating b))
	    (d (length (different-fixed a b))))
	(cond ((and (lset<= eq? a-fixed b-fixed)
		    (lset<= eq? b-floating a-floating)) (list 'inc> d))
	      ((and (lset<= eq? b-fixed a-fixed)
		    (lset<= eq? a-floating b-floating)) (list 'inc< d))
	      (else (%similarity a b))))
      (error "inclusion, wrong args" a b)))

(define (similarity a b)
  (if (andmap attractor? (list a b))
      (if (and (equal? (attractor-fixed a) (attractor-fixed b))
	       (null? (different-fixed a b)))
	  'eq
	  (inclusion a b))
      (error "similarity, wrong args:" a b)))

;;; Iterate SIMILARITY on a list of attractors.
(define (similarity-list attractors)
  (if (and (list? attractors)
	   (andmap attractor? attractors))
      (let loop ((x attractors)
		 (y '()))
	(if (null? x)
	    (reverse y)
	    (loop (cdr x)
		  (cons (map (lambda (b)
			       (similarity (car x) b))
			     (cdr x))
			y))))
      (error "similarity-list, wrong arg:" attractors)))

(define (similarity-matrix clst llst)
  (let* ((labels (make-labels (length clst) (length llst)))
	 (values (concatenate 
		  (similarity-list (read-attractor-list (append clst llst)))))
	 (results (append labels values))
	 (matrix (%fill-diagonal (list->simmetric-matrix results) 'eq)))
    (begin
      (array-set! matrix "" 0 0)
      matrix)))

;;; Return a weighted distance between basins.
;;; Given two sets of elements x_i and y_i:
;;; d = N_x/N_Y * sqrt ( sum (x_i - y_i)^2 )
(define (distance v1 v2)
  (let ((l1 (length v1))
	(l2 (length v2)))
    (if (< l1 l2)
	(let loop ((x (sort v1 <))
		   (y (sort v2 <))
		   (sum 0))
	  (if (null? x)
	      (sqrt (* (/ l2 l1) sum))
	      (loop (cdr x) (cdr y) (+ (* (- (car x) (car y))  
					  (- (car x) (car y)))
				       sum))))
	(let loop ((x (sort v2 <))
		   (y (sort v1 <))
		   (sum 0))
	  (if (null? x)
	      (sqrt (* (/ l1 l2) sum))
	      (loop (cdr x) (cdr y) (+ (* (- (car x) (car y))
					  (- (car x) (car y)))
				       sum)))))))

(define (basin-distance cyclic loose)
  (if (or (null? cyclic)
	  (null? loose))
      '()
      (let ((v1 (map attractor-basin cyclic))
	    (v2 (map attractor-basin loose)))
	(exact->inexact (distance v1 v2)))))

;;; Build a fixed digit index, ex: 000, 001 ... 999
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

;;; Run similarity on a sample.
(define (similarity-run prefix n)
  (let ((index (make-index n)))
    (let loop ((i index))
      (if (null? i)
	  (display "Evaluation complete.\n")
	  (let ((id (string-append prefix (car i) ".scm")))
	    (load id)
	    (display (string-append "Net id: " (car i) "\n\n"))
	    (display "Basin:\t")
	    (map (lambda (x)
		   (display (attractor-basin x))
		   (display "\t"))
		 (read-attractor-list (append cyclic loose)))
	    (newline)
	    (display-matrix (similarity-matrix cyclic loose))
	    (newline)
	    (loop (cdr i)))))))

;;;------------------------------------------------
;;; Comparison between cyclic and loose attractor.
;;;------------------------------------------------

;;; Cyclic - loose equality predicate.
(define (cl-equal? c l)
  (if (eq? (similarity c l) 'eq)
      #t
      #f))

;;; Return the number of occorence of SYMBOL in similarity
;;; between a cycle an a list of looses.
(define (symbol-counter cycle loose-lst symbol)
  (let loop ((l loose-lst)
	     (n 0))
    (if (null? l)
	n
	(if (eqv? symbol (similarity cycle (car l)))
	    (loop (cdr l) (+ 1 n))
	    (loop (cdr l) n)))))

;;; Return the number of equality.
(define (equal-counter cyclic loose)
  (let
      ((eq-counter
	(map
	 (lambda (c) 
	   (symbol-counter c loose 'eq))
	 cyclic)))
    (- (length eq-counter)
       (count zero? eq-counter))))

;;; Return number of inclusion.
(define (inclusion-counter cyclic loose)
  (let
      ((inc>-counter
	(map
	 (lambda (c)
	   (symbol-counter c loose 'inc>))
	 cyclic))
       (inc<-counter
	(map
	 (lambda (c)
	   (symbol-counter c loose 'inc<))
	 cyclic)))
    (- (length cyclic)
       (count zero?
	      (map
	       (lambda (x y) (+ x y))
	       inc>-counter
	       inc<-counter)))))

;;; Print a list with number of cycle, number of loose,
;;; number of cycle-loose equality, number of cycle-loose disjointness.
(define (%similarity-map cyclic loose)
  (let ((nc (length cyclic))
	(nl (length loose)))
    (cond ((zero? nl) (list nc '() '() '()))
	  ((zero? nc) (list '() nl '() '()))
	  (else (let* ((n-eq (equal-counter cyclic loose))
		       (n-inc (inclusion-counter cyclic loose))
		       (n-disj (- (max nc nl) (+ n-eq n-inc))))
		  (list nc nl n-eq n-disj))))))


(define (similarity-map cyclic loose)
  (%similarity-map (read-attractor-list cyclic) (read-attractor-list loose)))

(define (similarity-map-run prefix n)
  (let ((index (make-index n)))
    (let loop ((i index))
      (if (null? i)
	  (display "Evaluation complete.\n")
	  (let ((id (string-append prefix (car i) ".scm")))
	    (load id)
	    (display (car i))
	    (display #\tab)
	    (map
	     (lambda (x)
	       (display x)
	       (display #\tab))
	     (similarity-map cyclic loose))
	    (newline)
	    (loop (cdr i)))))))

;;; Run similarity and some statistics.
;;; NB: GET-AVERAGE returns a list (mean standard-deviation).
;;; File "similarity.readme" contain explanation on output format. 
(define (similarity-stats-map-run prefix n-nets)
  (let ((index (make-index n-nets)))
    (display "Id\tnc\tnl\teq\tdis\tpc\tds\tpl\tds\tbc\tds\tbl\tds\tb.dis\tfnc\t\ds\tfnl\tds\n")
    (let loop ((i index))
      (if (null? i)
	  (display "Evaluation complete.\n")
	  (let ((id (string-append prefix (car i) ".scm")))
	    (load id)
	    (let* ((a1 (read-attractor-list cyclic))
		   (a2 (read-attractor-list loose))
		   (p1 (get-average attractor-size a1))
		   (p2 (get-average attractor-size a2))
		   (b1 (get-average attractor-basin a1))
		   (b2 (get-average attractor-basin a2))
		   (bd (list (basin-distance a1 a2)))
		   (nf1 (get-average attractor-n-floating a1))
		   (nf2 (get-average attractor-n-floating a2)))
	      (display (car i))
	      (display #\tab)
	      (map
	       (lambda (x)
		 (if (inexact? x)
		     (format #t "~$" x)
		     (display x))
		 (display #\tab))
	       (append 
		(%similarity-map a1 a2)
		p1
		p2
		b1
		b2
		bd
		nf1
		nf2))
	      (newline)
	      (loop (cdr i))))))))

(define (similarity-stats-map-run/file prefix n-nets filename)
  (let ((out (open-output-file filename))
	(previous (current-output-port)))
    (if (output-port? out)
	(begin (set-current-output-port out)
	       (similarity-stats-map-run prefix n-nets)
	       (set-current-output-port previous)
	       (close-output-port out))
	(error "similarity-stats-map-run/file:cannot open port."))))


;;;-------------------------------------------------------------
;;; Return sizes (periods) of cycles and looses that are equals.
;;;-------------------------------------------------------------
(define (%sizeof-equals cyclic loose-list)
  (filter pair?
	  (map (lambda (x)
		 (if (cl-equal? cyclic x)
		     (cons (attractor-size cyclic)
			   (attractor-size x))))
	       loose-list)))

(define (sizeof-equals cyclic-list loose-list)
  (concatenate (map (lambda (x)
		      (%sizeof-equals x loose-list))
		    cyclic-list)))

(define (sizeof-equals-run prefix n)
  (let ((index (make-index n)))
    (let loop ((i index)
	       (results '()))
      (if (null? i)
	  (sort (delete-duplicates (concatenate results))
		(lambda (x y)
		  (cond
		   ((< (car x)
		       (car y)) #t)
		   ((and (= (car x) (car y))
			 (< (cdr x) (cdr y))) #t)
		   (else #f))))
	  (let ((id (string-append prefix (car i) ".scm")))
	    (load id)
	    (loop
	     (cdr i)
	     (cons
	      (sizeof-equals
	       (read-attractor-list cyclic)
	       (read-attractor-list loose))
	      results)))))))

(define (display-sizeof-equals-run prefix n)
  (let ((points (sizeof-equals-run prefix n)))
    (map (lambda (x)
	   (display (car x))
	   (display #\tab)
	   (display (cdr x))
	   (newline))
	 points)))
