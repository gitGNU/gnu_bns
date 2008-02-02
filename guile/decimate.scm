;;; decimate.scm - reduce a net to a more simple one

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

;;; Following srfi are used:
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))

;;; Some definition here:
(load "utilities.scm")
(load "stats.scm")
(load "net.scm")

;;; Decimation utilities.

;;; Explanation.
;;; F is a vector that represesent a boolean rule.
;;;
;;;   in_k ... in_1 in_0    out
;;;  ------------------------------
;;;    0   ...  0    0      F(0)
;;;    0   ...  0    1      F(1)
;;;   ...  ... ...  ...     ...
;;;    1   ...  1    1      F(m-1) where m = 2^k
;;;
;;; A and B are tensors defined as follows:
;;;
;;; A_ij : index of F for which Ith input = 0
;;;
;;;    j   |  in_0 in_1 ... in_I  ... in_k
;;;  --------------------------------------
;;;    0   |   0    0   ...  ...  ...  0
;;;    1   |   2    1   ...  ...  ...  1
;;;    2   |   4    2   ...  ...  ...  2
;;;    3   |   6    5   ...  ...  ...  3
;;;   ...  |  ...  ...  ...  ...  ...  4
;;;    J   |  ...  ...  ...  Aij  ... ...
;;;   ...  |  ...  ...  ...  ...  ... ...
;;;  m/2-1 |  2m  2m-1 ...  ...  ... m/2-1
;;;
;;; B_ij : index of F for which Ith input = 1
;;;
;;;    j   | in_0 in_1 ... in_I  ... in_k
;;;  --------------------------------------
;;;    0   |  1    2   ...  ...  ... m/2
;;;    1   |  3    3   ...  ...  ... m/2+1
;;;    2   |  5    6   ...  ...  ... m/2+2
;;;    3   |  7    7   ...  ...  ... m/2+3
;;;   ...  | ...  ...  ...  ...  ... m/2+4
;;;    J   | ...  ...  ...  Bij  ... ...
;;;   ...  | ...  ...  ...  ...  ... ...
;;;  m/2-1 | 2m+1 2m+1 ...  ...  ... m/2+1
;;;
;;; I is index of inputs.
;;; J are half of (- (expt 2 K) 1) possible values of F.
;;;
;;; F(a_ij) = F(b_ij) each J ==> Ith inputs is unused.
;;;

(define (decimation-table-a i j)
  (- (* j 2)
     (modulo j (expt 2 i))))

(define (decimation-table-b i j)
  (- (+ (* j 2)
	(expt 2 i))
     (modulo j (expt 2 i))))

(define (unused-input? rule i)
  (let* ((n-iter (/ (vector-length rule) 2)))
    (let loop ((j 0))
      (if (= j n-iter)
	  #t
	  (if (eqv? (vector-ref rule (decimation-table-a i j)) 
		    (vector-ref rule (decimation-table-b i j)))
	      (loop (+ j 1))
	      #f)))))

;;; Return a rule equivalent to RULE with I-th inputs fixed to CONST.
;;; CONST is table A (for 0) or B (for 1)
(define (%set-entry-to-const rule i const)
  (let loop ((new '())
	     (j 0))
    (if (= j (/ (vector-length rule) 2))
	(list->vector (reverse new))
	(loop (cons (vector-ref rule (const i j)) new)
	      (+ 1 j)))))

;;; Return a rule equivalent ro RULE with I-th inputs fixed to 0. 
(define (%set-const-entry-to-0 rule i)
  (%set-entry-to-const rule i decimation-table-a))

;;; Return a rule equivalent ro RULE with I-th inputs fixed to 1. 
(define (%set-const-entry-to-1 rule i)
  (%set-entry-to-const rule i decimation-table-b))

;;; If an input is unused, it can be removed setting it to 0 or 1
;;; (resulting rule is the same for 0 or 1).
(define remove-rule-entry %set-const-entry-to-0)

;;; Set an input to a costant value.
;;; Needed to fill inputs of removed node's descendants.
(define (set-costant-input! node i fun) ; fun is DECIMATION-TABLE-A or DECIMATION-TABLE-B
  (let* ((old-f (node-f node))
	 (old-in (node-in node))
	 (new-f (%set-entry-to-const old-f i fun))
	 (new-in (remove-list-item old-in i)))
    (begin
      (node-set-f! node new-f)
      (node-set-in! node new-in)
      node)))  

;;; Remove I-th input from NODE.
(define (remove-input! node i)
  (let* ((old-f (node-f node))
	 (old-in (node-in node))
	 (new-f (remove-rule-entry old-f i))
	 (new-in (remove-list-item old-in i)))
    (begin
      (node-set-f! node new-f)
      (node-set-in! node new-in)
      node)))

;;; Check each NODE's input and remove it if unused.
(define (remove-unused-inputs! node)
  (cond
    ((null? node) '())
    ((node? node) (let loop ((nd node)
                             (i 0))
                    (let ((len (length (node-in nd)))
                          (rule (node-f nd)))
                      (if (eqv? i len)
                          '()
                          (if (unused-input? rule i)
			      (loop (remove-input! nd i) i)
                              (loop nd (+ 1 i)))))))
    (else (error "remove-unused-inputs!: wrong arg"))))

;;; Return DECIMATION-TABLE-A if (eqv? VALUE 0), DECIMATION-TABLE-B otherwise.
(define (%fun-selector value)
  (if (eqv? value 0)
      decimation-table-a
      decimation-table-b))

;;; If NODE-IDth node in NET has no input, his rules has a single value (2^0 = 1)
;;; Set his value as constant input to his descendant.
(define (remove-if-no-inputs! node-id net)
  (let ((node (net-ref net node-id))) ; get node
    (cond
      ((null? node) '())
      ((node? node)
       (if (and (null? (node-in node))
                (eq? 1 (vector-length (node-f node)))) ; node has no inputs
	   (let ((fun (%fun-selector (vector-ref (node-f node) 0))))
	     (let loop ((out (node-outputs node-id net)))
	       (if (null? out)
		   '()
		   (let* ((out-nd (net-ref net (car out)))
			  (index (list-index (lambda (x)
					       (if (eqv? node-id x) #t #f)) 
					     (node-in out-nd))))
		     (begin
		       (set-costant-input! out-nd index fun)
		       (loop (cdr out))))))
	     (net-set! net node-id '()))))
      (else "remove-if-no-inputs!: wrong args"))))

;;; If NODE_IDth node of NET has no outputs, simply remove it.
(define (remove-if-no-outputs! node-id net)
  (let* ((node (net-ref net node-id))
	 (outputs (node-outputs node-id net)))
    (if (null? outputs)
	(net-set! net node-id '()))))

;;; Decimate NET
(define (decimate-net net)
  (let loop ((old (net-copy net))
             (current (net-copy net))
             (flag #f)) ; needed to force condition to #f the first time
    (if (and (net-equal? old current) flag)
        (let out-loop ((i 0))       ; remove all nodes with no outputs
	  (if (eqv? i (net-length current))
	      current
	      (begin
		(remove-if-no-outputs! i current)
		(out-loop (+ 1 i)))))
        (loop (net-copy current)
              (let nd-loop ((i 0))
                (if (eqv? i (net-length current))
                    current
                    (begin
		      (remove-unused-inputs! (net-ref current i))
                      (remove-if-no-inputs! i current)
                      (nd-loop (+ 1 i)))))
              #t))))

;;; Net mapping functions: write a new net (without "holes")
;;; from a decimated one.

;;; Build a index list: elements are old node ids, indeces are new ones.
(define (make-index-map net)
  (let loop ((i 0)
	     (index '()))
    (cond ((eqv? i (net-length net)) (reverse index))
	  ((null? (net-ref net i)) (loop (+ 1 i) index))
	  (else (loop (+ 1 i) (cons i index))))))

;;; Return a new list of inputs mapped from IN throught INDEX-MAP
(define (map-inputs in index-map)
  (let loop ((old in)
	     (new '()))
    (if (null? old)
	(reverse new)
	(loop (cdr old)
	      (cons (list-index (lambda (x) 
				  (if (eqv? x (car old)) #t #f))
				index-map)
		    new)))))

;;; Set NODE's new inputs throught INDEX-MAP
(define (remap-node! node index-map)
  (begin
    (node-set-in! node (map-inputs (node-in node) index-map))
    node))

;;; Return a new net equal to NET but with all nodes remapped.
(define (remap-net net)
  (let* ((index (make-index-map net))
	 (old-nodes (delete '() (map cdr (net-nodes net))))
	 (new-nodes (map (lambda (x)
                           (if (node? x)
                               (remap-node! x index)))
			 old-nodes)))
    (make-net new-nodes)))

;;; Remap initial conditions

;;; INIT is a single initial condition. INDEX is a map returned by MAKE-INDEX-MAP.
;;; Return a new initial condition without nodes that are decimated.
(define (decimate-init init index)
  (let loop ((i index)
	     (new '()))
    (if (null? i)
	(reverse new)
	(loop (cdr i) (cons (vector-ref init (car i)) new)))))

;;; Write a list of decimated initial condition to PORT.
;;; Original initial conditions are elements of INITS-LIST.
(define (save-decimated-init inits-list index port)
  (map (lambda (x)
	 (display-list (decimate-init x index) port)
	 (newline port))
       inits-list))

;;; Write the map between original and decimated net to PORT.
(define (save-index-map index-list port)
  (map (lambda (x)
	 (display-list-with-space x port)
	 (newline port))
       index-list))

;;; Read maps of N decimated nets from PATH.
(define (read-index-maps path n)
  (let ((port (open-input-file path)))
    (let loop ((i 0)
               (index-map '()))
      (if (eq? i n)
          (begin
            (close-input-port port)
            (reverse index-map))
          (loop (+ 1 i) (cons (read-list port) index-map))))))

; Decimate nets remapping inital condition.
(define (read-inits init-path n-init)
  (let ((i-port (open-input-file init-path)))
    (let loop ((inits '())
	       (i 0))
      (if (eqv? i n-init)
	  (reverse inits)
	  (loop (cons (list->vector (read-int-list i-port))
		      inits)
		(+ 1 i))))))

;;; Decimate N nets reading them from files in IN-PATH and put results in OUT-PATH.
;;; Return filename of map's file.
(define (decimate-run in-path out-path n)
  (let* ((old-nets (read-nets in-path n))
         (nets (map decimate-net old-nets))
         (nets-id (make-index n))
         (index (map make-index-map nets))
	 (map-path (string-append out-path "d.map"))
         (map-port (open-output-file map-path)))
    (save-index-map index map-port)
    (close-output-port map-port)
    (map (lambda (x y)
           (save-net (remap-net x)  (string-append out-path y)))
         nets nets-id)
    map-path))

;;; Read N initial condition from INITS-PATH, remap them
;;; (throught a index map readed from MAP-PATH, and put
;;; resulting initial condition in OUT-PATH.
(define (decimate-inits inits-path out-path map-path n)
  (let ((inits-list (read-inits inits-path n))
	(maps-list (read-index-maps  map-path n))
	(port-list (map (lambda (x)
			  (open-output-file
			   (string-append out-path x ".ini")))
			(make-index n))))
    (map (lambda (x p)
	   (save-decimated-init inits-list x p)
	   (close-output-port p))
	 maps-list
	 port-list)))

;;; Decimate N nets from IN-PATH, putting resulted nets in OUT-PATH.
;;; For each net make also a file of decimated inits from INITS-PATH.
(define (decimate-run-with-inits in-path out-path n inits-path)
  (decimate-inits inits-path out-path (decimate-run in-path out-path n) n))

;;; Display N and K for each of N nets in PATH.
(define (decimation-report path n)
  (let* ((nets (read-nets path n))
         (len (map net-length nets))
         (k (map (lambda (x)
                   (if (zero? (net-length x))
                       0
                       (net-k x))) nets)))
    (map (lambda (i x y)
           (begin (display i)
                  (display #\tab)
                  (display x)
                  (display #\tab)
                  (display y)
                  (newline))) (iota n) len k)))

;;; Display average N and average K for N nets in PATH.
(define (net-statistics path n)
  (let* ((nets (read-nets path n))
         (len (map net-length nets))
         (k (map (lambda (x)
                   (if (zero? (net-length x))
                       0
                       (net-k x))) nets)))
    (display (cons (exact->inexact (mean len)) (mean k)))))
