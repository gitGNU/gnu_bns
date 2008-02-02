;;; attractor.scm - definition of attractors and utilities to work with it.

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

;;; Attractors for Random Boolean Networks.
;;; This file give a representation of attractors and some operator
;;; to work with them.
;;; An attractor is a set of states.
;;; The rapresentation is made of some parameters, it doesn't contain all
;;; states.
;;; If two attractors has a state in common they are identical.
;;; The ID of each attractor is the state (vector of boolean values) who
;;; has greatest value if seen as binary rapresentation of a number.

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))

(load "utilities.scm")

;;; Record definition.
(define-record-type :attractor
  (really-make-attractor id size basin fixed floating)
  attractor?
  (id attractor-id) ;;; id (vector of boolean values: "name" of attractor)
  (size attractor-size) ;;; number of state in attractor
  (basin attractor-basin) ;;; number of state that evolve to attractor
  (fixed attractor-fixed) ;;; fixed nodes (same value in each state)
  (floating attractor-floating)) ;;; floating nodes

;;; Constructor. If ALIST is '() create a empty attractor.
;;; ALIST is:
;;; ((id . <vector>)
;;;  (period . <int>)
;;;  (basin . <int>)
;;;  (fixed . <list of ints between 0 and N-1>)
;;;  (floating <list of ints between 0 and N-1>))
;;; NOTE: intersection between FIXED and FLOATING must be null.
(define (make-attractor alist)
  (cond 
    ((and (list? alist) (eq? 5 (length alist)))
         (let ((id (cdr (assq 'id alist)))
	  (size (cdr (assq 'period alist)))
	  (basin (cdr (assq 'basin alist)))
	  (fixed (cdr (assq 'fixed alist)))
	  (floating (cdr (assq 'floating alist))))
      (cond ((not (vector? id))
	     (error "make-attractor, wrong id: " id))
	    ((not (number? size))
	     (error "make-attractor, wrong size: " size))
	    ((not (number? basin))
	     (error "make-attractor, wrong basin: " basin))
	    ((not (list? fixed))
	     (error "make-attractor, wrong fixed nodes: " fixed))
	    ((not (list? floating))
	     (error "make-attractor, wrong floating nodes:" floating))
	    ((not (null? (lset-intersection eq? fixed floating)))
	     (error "make-attractor: fixed and floating intersection not null."))
	    (else (really-make-attractor id size basin fixed floating)))))
   ((null? alist) (really-make-attractor '#() 0 0 '() '()))
   (else (error "make-attractor, wrong type:" alist))))

;;;---------------------------------
;;; Some others accessors.
;;;---------------------------------

;;; Number of fixed nodes.
(define (attractor-n-fixed a)
  (if (attractor? a)
      (length (attractor-fixed a))
      (error "attractor-n-fixed, wrong arg:" a)))

;;; Number of floating nodes.
(define (attractor-n-floating a)
  (if (attractor? a)
      (length (attractor-floating a))
      (error "attractor-n-floating, wrong arg:" a)))

;;; Fixed nodes associated with their boolean value.
;;; Return a list of pairs: (<node id> . <boolean value>).
(define (attractor-fixed-nodes-value a)
  (if (attractor? a)
      (let ((x (attractor-id a))
	    (fixed (attractor-fixed a)))
	(let loop ((fn fixed)
		   (alist '()))
	  (if (null? fn)
	      (reverse alist)
	      (let* ((node (car fn))
		     (rest (cdr fn))
		     (value (vector-ref x node))
		     (elt (cons node value)))
		(loop rest (cons elt alist))))))
      (error "attractor-fixed-nodes-value, should be an attractor: " a)))

;;;---------------------------------
;;; Binary operators on attractors.
;;;---------------------------------

;;; Floating nodes in common between A and B.
(define (floating-intersection a b)
  (if (andmap attractor? (list a b)) 
      (let ((fn1 (attractor-floating a))
	    (fn2 (attractor-floating b)))
	(lset-intersection eq? fn1 fn2))
      (error "floating-intersection, should be attractors: " a b)))

;;; Union between floating nodes in A and floating nodes in B.
(define (floating-union a b)
  (if (andmap attractor? (list a b)) 
      (let ((fn1 (attractor-floating a))
	    (fn2 (attractor-floating b)))
	(lset-union eq? fn1 fn2))
      (error "floating-union, should be attractors: " a b)))

;;; Fixed nodes in common between A and B.
(define (outer-core a b)
  (if (andmap attractor? (list a b)) 
      (let ((fn1 (attractor-fixed a))
	    (fn2 (attractor-fixed b)))
	(lset-intersection eq? fn1 fn2))
      (error "outer-core, should be attractors: " a b)))

;;; Fixed nodes in common between A and B that have the same binary value.
(define (inner-core a b)
  (if (andmap attractor? (list a b)) 
      (let* ((fnv1 (attractor-fixed-nodes-value a))
	     (fnv2 (attractor-fixed-nodes-value b))
	     (intersection (lset-intersection equal? fnv1 fnv2)))
	(map car intersection))
      (error "inner-core, should be attractors: " a b)))

;;; Number of nodes that have different value in A and B.
(define (inner-complement-length a b)
  (if (andmap attractor? (list a b)) 
      (- (vector-length (attractor-id a))
	 (length (inner-core a b)))
      (error "inner-complement-length, should be attractors: " a b)))

;;; Union between fixed nodes in A and fixed nodes in B.
(define (fixed-union a b)
  (if (andmap attractor? (list a b)) 
      (let ((fn1 (attractor-fixed a))
	    (fn2 (attractor-fixed b)))
	(lset-union eq? fn1 fn2))
      (error "fixed-union, should be attractors: " a b)))

;;; Fixed nodes in common between A and B but with different value.
(define (different-fixed a b)
  (if (andmap attractor? (list a b)) 
      (let ((outer (outer-core a b))
	    (inner (inner-core a b)))
	(lset-difference eq? outer inner))
      (error "different-fixed, should be attractors: " a b)))

;;;---------------------------------
;;; Lists of attractors.
;;;---------------------------------

;;; Read a list of alists defining attractors. Return list of attractors.
(define (read-attractor-list elts)
  (cond
   ((list? elts) (map make-attractor elts))
   ((null? elts) '())
   (else (error "read-attractor-list, wrong args:" elts))))

;;; Sorting lists of attractors.

;;; Define minority relationship.
(define (attractor< a1 a2 accessor)
  (if (< (accessor a1)
	 (accessor a2))
      #t
      #f))

;;; Sort by attributes (from min to max).
(define (attractors-sort-by attractors-list accessor)
  (sort attractors-list accessor))

;;; Sort by size.
(define (attractors-sort-by-size attractors-list)
  (attractors-sort-by attractors-list attractor-size))

;;; Sort by basin.
(define (attractors-sort-by-basin attractors-list)
  (attractors-sort-by attractors-list attractor-basin))
