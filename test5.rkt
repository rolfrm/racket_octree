#lang racket
(define (parse-number s)
  (cond
   [(equal? "1" s) 1]
   [(equal? "2" s) 2]
   [else 0]))

(parse-number "1")

(require "octree-base.rkt")
(display (get-payload (create-node 3)))

;(define (get-relative-node2 node x y z idx)
;  ())

(define eight '(0 1 2 3 4 5 6 7))

(define (get-node-index n)
  (let ((parent (get-parent n)))
    (for/first ([i '(0 1 2 3 4 5 6 7)]
		#:when 
		(and (has-child-node parent i) 
		     (equal? n (get-child-node parent i))))
	       i)
    ))

(define (int-div x y)
  (floor (/ x y)))

(define (get-relative-node node x y z)
  (let ((idx (get-node-index node)))
    (let ((ly (bitwise-bit-field idx 1 2))
	  (lx (bitwise-bit-field idx 0 1))
	  (lz (bitwise-bit-field idx 2 3))
	  (parent (get-parent node)))
      (let ((nx (int-div (+ x lx) 2))
	    (ny (int-div (+ y ly) 2))
	    (nz (int-div (+ z lz) 2)))
	(let ((nparent (if (and (eq? nx 0) (eq? ny 0) (eq? nz 0))
			   parent
			   (get-relative-node parent nx ny nz))))
	  (let ((l2x (bitwise-and (+ lx x) 1))
		(l2y (bitwise-and (+ ly y) 1))
		(l2z (bitwise-and (+ lz z) 1)))
	    (let ((nidx (+ l2x (* l2y 2) (* l2z 4))))
	      (get-child-node nparent nidx))))))))
(print "----")
(newline)
(define n1 (create-node 1))
(define nodes null)
(for ([i (in-range -10 11)])
     (set! nodes (cons (get-relative-node n1 i i i) nodes))
     (set-payload (first nodes) i)
     )
(for ([i (in-range -10 11)])
     (printf ">> ~a\n" (get-payload (get-relative-node n1 i i i)))
     )
(define n2 (get-relative-node n1 10 2 3))
(define n3 (get-relative-node n2 -10 -2 -3))
(define n4 (get-relative-node n1 0 0 0))
(print (equal? n1 n3)) 
(print (equal? n4 n1))
(newline)
(for ([i (in-range -10 11)])
     (printf ">> ~a ~a\n" (int-div i 2) (quotient i 2)))

(define (render-node node size f x y z)
  (let ((s (* size 0.5)))
    (for ([i (in-range 8)])
	 (when (has-child-node node i)
	       (let ((ly (bitwise-bit-field i 1 2))
		     (lx (bitwise-bit-field i 0 1))
		     (lz (bitwise-bit-field i 2 3)))
		 (let ((nx (+ x (* lx s)))
		       (ny (+ y (* ly s)))
		       (nz (+ z (* lz s))))
		   (f (get-child-node node i) nx ny nz s)
		   (render-node (get-child-node node i) s f nx ny nz)))))))


(define top-parent n1)
(for ([i (in-naturals)]
      #:break (not (has-parent top-parent)))
     (print (has-parent top-parent))
     (newline)
     (set! top-parent (get-parent top-parent)))
(has-parent top-parent)

(render-node top-parent 1.0 
	     (lambda (node x y z s) 
	       (unless (null? (get-payload node))
		       (print (list x y z s (get-payload node)))
		       (newline)))
	     0 0 0)
