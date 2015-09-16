#lang racket/gui
(require racket/gui/base)
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
(for ([i (in-range -5 5)])
     (set! nodes (cons (get-relative-node n1 i 0 0) nodes))
     (set-payload (first nodes) i)
     (set! nodes (cons (get-relative-node n1 0 0 i) nodes))
     (set-payload (first nodes) i)
     (set! nodes (cons (get-relative-node n1 0 i 0) nodes))
     (set-payload (first nodes) i)
     )
(define n5 (get-parent (get-parent (get-parent n1))))
(define n6 (get-relative-node n5 5 2 2))
(set-payload n6 1)
;(for ([i (in-range -10 11)])
;     (printf ">> ~a\n" (get-payload (get-relative-node n1 i i i)))
					;     )
(define n2 (get-relative-node n1 1 0 0))
(define n3 (get-relative-node n2 -10 -2 -3))
(define n4 (get-relative-node n1 0 0 0))
(print (equal? n1 n3)) 
(print (equal? n4 n1))
(newline)
(for ([i (in-range -10 11)])
     (printf ">> ~a ~a\n" (int-div i 2) (quotient i 2)))

(define (render-node node size f x y z)
  (let ((s (/ size 2)))
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

;; Calculate the position relative to the parent node
 (define (get-parent-offset node s p)
   (let ((idx (get-node-index node))
	 (parent (get-parent node)))
     (let ((ly (bitwise-bit-field idx 1 2))
 	  (lx (bitwise-bit-field idx 0 1))
 	  (lz (bitwise-bit-field idx 2 3)))
       (let ((rx  (* s (- lx 1)))
	     (ry  (* s (- ly 1)))
	     (rz  (* s (- lz 1))))
	 (if (equal? parent p)
	     (values rx ry rz)
	     (let-values ([(px py pz) (get-parent-offset parent (* s 2) p)])
	       (values (+ rx px) (+ ry py) (+ rz pz))))))))
	     
(define (get-parent-tree node)
  (cons node
	(if (has-parent node)
	    (get-parent-tree (get-parent node))
	    null)))
      

(define (find-common-parent nodea nodeb)
  (let ((ta (reverse (get-parent-tree nodea)))
	(tb (reverse (get-parent-tree nodeb))))
    (for/last ([a ta]
	       [b tb]
	       #:when (equal? a b))
	      a)))

;; Base item size should never change as this will decrease visual quality.
;; Since its isometric there is a mismatch between the way stuff is understood
;; and the way it is rendered.
;; To fix this, an offset can be attached to each tile sprite.
;; That offset can be used for sprites that are smaller or larger than their designated cube.
;; Note that most things are bigger than their designated cubes

(define (iso-offset x y z)
  (values (+ x z)
	  (- (/ z 2) y (/ x 2) )))


(define na (create-node 1))
(define nb (get-relative-node na 1 0 0))
(let-values ([(x y z) (get-parent-offset na 1 (find-common-parent na nb))])
  (print (iso-offset x y z)))
(let-values ([(x y z) (get-parent-offset nb 1 (find-common-parent na nb))])
  (print (iso-offset x y z)))

(newline)
(exit)


(struct sprite
	(image x y))

(define tile (sprite (read-bitmap "tile.png") 0 -5))
(define frame (new frame% [label "Example"] [width 512] [height 512]))  
;(define msg (new message% [parent frame]))

(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
 ;   (define/override (on-event event)
 ;     (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
 ;   (define/override (on-char event)
 ;     (send msg set-label "Canvas keyboard"))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
(new my-canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
	;(send dc set-brush (send the-brush-list find-or-create-brush 
	;			 (make-object color% 0 0 0 1.0) 'solid))
	;(let-values ([(x y) (send canvas get-size)])
	(let ((x 1024) (y 1024))
			       ;(send canvas set-canvas-background (make-object color% 255 0 0 0))
	  (render-node (get-parent (get-parent (get-parent (get-parent (get-parent n1))))) (max x y)
		       (lambda (node x y z s) 
			 (let-values ([(nx ny) (iso-offset x y z)])
			   (unless (null? (get-payload node))
				   (printf "(~a ~a ~a) -> (~a ~a)\n" x y z nx ny)
				   ;(send dc draw-rectangle nx (+ ny 512) s s)
				   (send dc draw-bitmap (sprite-image tile) 
					 (+ nx 0 (sprite-x tile)) (+ ny (sprite-y tile) 512)))))
		       0 0 0)
	  )
	;(send dc draw-text "Dont panic" 0 0)
	)])

(send frame show #t)
