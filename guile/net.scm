;;; net.scm - definition of node and net

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
(use-modules (srfi srfi-9))

(load "utilities.scm")
(load "stats.scm")

;;; Miscellaneous utilities working on lists.
;;; Functions for reading inputs: READ_LIST read each line as a space-separated
;;; list, READ-CHAR_LIST read each char (except #\newline) as a list element.
;;; REMOVE-LIST-ITEM remove a list element by index.

(define (%read-list port f)
  (let loop ((v '()))
    (case (peek-char port)
      ((#\newline) (begin (read-char port)
			  (reverse v)))
      ((#\space) (begin (read-char port)
			(loop v)))
      (else (loop (cons (f port) v))))))

(define (read-list port)
  (%read-list port read))

(define (read-char-list port)
  (%read-list port read-char))

(define (read-int-list port)
  (%read-list port (lambda (x) (- (char->integer (read-char x)) 48))))

(define (display-list lst port)
  (if (list? lst)
      (map (lambda (x) (display x port)) lst)
      (error "wrong type")))

(define (display-list-with-space lst port)
  (if (list? lst)
      (map (lambda (x) (display x port) (display #\space port)) lst)
      (error "wrong type")))

(define (remove-list-item l k)
  (if (null? l)
      '()
      (if (< k (length l))
	  (let ((first (reverse (list-tail (reverse l) (- (length l) k))))
		(last (cdr (list-tail l k))))
	    (append first last)))))

(define (remove-vector-item v k)
  (if (vector? v)
      (list->vector (remove-list-item (vector->list v) k))))

;;; Record types definitions (srfi-9 with srfi-23 for errors).

(define-record-type :node
  (really-make-node f-vec in-lst)
  node?
  (f-vec node-f node-set-f!) ; boolean function
  (in-lst node-in node-set-in!)) ; inputs

(define (make-node f in)
  (if (and (vector? f)
	   (list? in))
      (really-make-node f in)
      (error "make-node: wrong argument.")))

; (define (node-copy node)
;   (if (node? node)
;       (really-make-node (list->vector (list-copy (vector->list (node-f node))))
; 			(list-copy (node-in node)))
;       (error "node-copy: wrong arg")))

(define (node-copy node)
  (cond
   ((null? node) '())
   ((node? node) (really-make-node (list->vector
				    (list-copy (vector->list (node-f node))))
				   (list-copy (node-in node))))
   (else (error "node-copy: wrong arg"))))

(define (node-equal? node1 node2)
  (cond
   ((and (null? node1) (null? node2))
    #t)
   ((and (node? node1) (node? node2))
    (and (equal? (node-f node1) (node-f node2))
	 (equal? (node-in node1) (node-in node2))))
   (else #f)))

(define (display-node node)
  (if (node? node)
      (begin (display "f: ") 
	     (display (node-f node))
	     (display " in: ")
	     (display (node-in node)))))

(define-record-type :net
  (really-make-net nodes)
  net?
  (nodes net-nodes net-nodes-set!)) ; alist of index-nodes pairs

(define (make-net lst)
  (if (and (list? lst))
;           (andmap node? lst))
      (let* ((n (length lst))
	     (indices (iota n)))
	(really-make-net (map cons indices lst)))
      (error "make-net: wrong argument")))

; (define (net-copy net)
;   (if (net? net)
;       (really-make-net (alist-copy (net-nodes net)))
;       (error "net-copy : wrong arg")))

; (define (net-copy net)
;   (if (net? net)
;       (let loop ((old (map cdr (net-nodes net)))
; 		 (new '()))
; 	(if (null? old)
; 	    (make-net new)
; 	    (loop (cdr old) (cons (node-copy (car old)) new))))
;       (error "net-copy : wrong arg")))

(define (net-copy net)
  (if (net? net)
      (let ((nodes (map (lambda (x) (node-copy (cdr x))) (net-nodes net))))
	(make-net (list-copy nodes)))))

(define (net-length net)
  (if (net? net)
      (length (net-nodes net))
      (error "net-length: wrong argument")))

(define (fast-net-ref net i)
  (cdr (assv i (net-nodes net))))

(define (fast-net-set! net i node)
  (set-cdr! (assv i (net-nodes net)) node))

(define (net-ref net i)
  (if (and (net? net)
	   (number? i))
      (fast-net-ref net i)
      (error "net-ref:wrong argument")))

(define (net-set! net i node)
  (if (and (net? net)
	   (or (node? node)
               (null? node)))
      (fast-net-set! net i node)
      (error "net-set?: wrong argument")))

; (define (net-equal? net1 net2)
;   (if (and (net? net1)
; 	   (net? net2))
;       (equal? (net-nodes net1) (net-nodes net2))
;       (error "net= : wrong arg")))

(define (net-equal? net1 net2)
  (if (and (net? net1)
	   (net? net2))
      (andmap node-equal?
	      (map (lambda (x) (cdr x))
		   (net-nodes net1))
	      (map (lambda (x) (cdr x))
		   (net-nodes net2)))))

(define (net-k net)
  (let ((nodes (map cdr (net-nodes net))))
    (let loop ((elts nodes)
               (k '()))
      (if (null? elts)
          (exact->inexact (mean k))
          (loop (cdr elts) (cons (length (node-in (car elts))) k))))))
	

; Return an alist of elements (from . to), like edges set in direct graphs.

(define (net-graph net)
  (let loop ((elts (net-nodes net))
	     (topology '()))
    (if (null? elts)
	topology
	(let ((node (car elts)))
          (if (null? (cdr node))
	      (loop (cdr elts) topology)
              (let ((to (car node)) ; integer
                    (from (node-in (cdr node)))) ;list
                (loop (cdr elts)
                      (append (map (lambda (x) (cons x to)) from)
			      topology))))))))

;;; Return outputs of NODE-IDth node in NET.
(define (node-outputs node-id net)
  (let ((graph (net-graph net))
	(graph-tail (lambda (x)
		      (let ((e (assv node-id x)))
			(find-tail (lambda (y) (eqv? e y)) x)))))
    (let loop ((elts (graph-tail graph))
	       (outputs '()))
      (if (eq? #f elts)
	  (reverse outputs)
	  (loop (graph-tail (cdr elts))
		(cons (cdar elts) outputs))))))
	     

(define (display-net net)
  (if (net? net)
      (for-each (lambda (x) 
		  (if (null? x)
		      (newline)
		      (begin
			(display "Node")
			(display (car x))
			(display ":   ")
			(display-node (cdr x))
			(newline))))
		(net-nodes net))
      (error "display-net: wrong argument")))



;;; Building data structures from file.

(define (read-node t-port f-port)
  (let ((f (map (lambda (x)
		  (- (char->integer x) 48))
		(read-char-list f-port)))
	(k-in (read-list t-port))
	(inputs (read-list t-port))
	(k-out (read-list t-port))
	(outputs (read-list t-port))
	(dummy (read-list t-port)))
    (make-node (list->vector f) inputs)))

(define (%read-header port)
  (let ((prms (list (read port) (read port) (read port))))
    (begin
      (read-char port)
      (read-char port)
      prms)))

(define (%read-net-files t-port f-port)
  (let ((prms (%read-header t-port)))
    (let loop ((i 0)
	       (net '()))
      (if (eqv? i (car prms))
	  (reverse net)
	  (loop (+ i 1) (cons (read-node t-port f-port) net))))))

(define (read-net name)
  (let* ((t-port (open-input-file (string-append name ".top")))
	 (f-port (open-input-file (string-append name ".rul")))
	 (net (%read-net-files t-port f-port)))
    (begin
      (close-input-port t-port)
      (close-input-port f-port)
      (make-net net))))

(define (read-nets path n)
  (let ((index (make-index n)))
    (map (lambda (x)
           (read-net (string-append path x)))
         index)))


;;; Storing data structures to files.

(define (save-header n t-port)
  (begin
    (display n t-port)
    (display "\n0\n0\n\n" t-port)))

(define (save-node net node-id t-port f-port)
  (let ((node (net-ref net node-id)))
    (cond
      ((null? node)
       (begin
	 (display 0 t-port)
	 (display "\n\n" t-port)
	 (display 0 t-port)
	 (display "\n\n\n" t-port)
	 (display "\n" f-port)))
      ((node? node)
       (let ((in (sort (node-in node) <))
	     (out (sort (node-outputs node-id net) <))
	     (f (node-f node)))
	 (begin
	   (display (length in) t-port) ; Display IN on T-PORT.
	   (display #\newline t-port)
	   (map (lambda (x)
		  (begin (display x t-port) (display #\space t-port)))
		in)
	   (display #\newline t-port)
	   (display (length out) t-port) ; Display OUT on T-PORT.
	   (display #\newline t-port)
	   (map (lambda (x)
		  (begin (display x t-port) (display #\space t-port)))
		out)
	   (display #\newline t-port)
	   (display #\newline t-port)
	   (map (lambda (x)
		  (display x f-port)) 
		(vector->list f)) ; Display F on F_PORT.
	   (display #\newline f-port))))
      (else (error "Not a node.")))))

(define (save-net net filename)
  (let ((n (net-length net))
	(t-port (open-output-file (string-append filename ".top")))
	(f-port (open-output-file (string-append filename ".rul"))))
    (begin
      (save-header n t-port)
      (map (lambda (x)
		  (save-node net x t-port f-port))
		(iota n))
      (close-output-port t-port)
      (close-output-port f-port))))
