#lang racket/gui
(require racket/gui/base)
(define (parse-number s)
  (cond
   [(equal? "1" s) 1]
   [(equal? "2" s) 2]
   [else 0]))

(parse-number "1")

(require "octree-base.rkt")

(define (get-node-index n)
  (let ((parent (get-parent n)))
    (for/first ([i (in-range 8)]
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
(define (render-node node size f x y z)
  (f node x y z size)
  (let ((s (/ size 2)))
    ;;it: xz: (1 0) (0 0) (1 1) (1 0)
    ;;        1     0     5     4
    ;;        3     2     7     6
    (for ([i '(1 0 5 4 3 2 7 6)])
	 (when (has-child-node node i)
	       (let ((ly (bitwise-bit-field i 1 2))
		     (lx (bitwise-bit-field i 0 1))
		     (lz (bitwise-bit-field i 2 3)))
		 (let ((nx (+ x (* lx s)))
		       (ny (+ y (* ly s)))
		       (nz (+ z (* lz s))))
		   (render-node (get-child-node node i) s f nx ny nz)
		   
		   ))))))

;; Calculate the position relative to the parent node
 (define (get-parent-offset node s p)
   (let ((idx (get-node-index node))
	 (parent (get-parent node)))
     (let ((ly (bitwise-bit-field idx 1 2))
 	  (lx (bitwise-bit-field idx 0 1))
 	  (lz (bitwise-bit-field idx 2 3)))
       (let ((rx  (* s lx -1))
	     (ry  (* s ly -1))
	     (rz  (* s lz -1)))
	 (if (equal? parent p)
	     (values rx ry rz)
	     (let-values ([(px py pz) (get-parent-offset parent (* s 2) p)])
	       (values (+ px rx) (+ py ry) (+ pz rz))))))))

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
	  (- (/ z 2) (+ y (/ x 2)) )))

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

(struct sprite (image x y))
(define tile (sprite (read-bitmap "tile2.png") 0 -12))
(define tile2 (sprite (read-bitmap "tile2x.png") 0 -23));(sprite (read-bitmap "tree.png") 0 -14))

(define n1 (get-child-node (get-child-node (create-node) 7) 0))
(set-payload n1 tile)
(define nodes null)
;; (for ([i (in-range -5 5)])
;;      (for ([j (in-range -5 5)])
;; 	  (set! nodes (cons (get-relative-node n1 i 0 j) nodes))
;; 	  (set-payload (first nodes) tile))
;;      ;(set! nodes (cons (get-relative-node n1 0 0 i) nodes))
;;      ;(set-payload (first nodes) i)
;;      (for ([j (in-range 6)])
;; 	  ;(print j) (newline)
;; 	  (set! nodes (cons (get-relative-node n1 j i j)
;; 			    nodes))
;; 	  (set-payload (first nodes) tile)
;; 	  )
     
;;      (set! nodes (cons (get-relative-node n1 2 i 2) nodes))
;;      (set-payload (first nodes) tile)
;;      (set! nodes (cons (get-relative-node n1 4 i 4) nodes))
;;      (set-payload (first nodes) tile)
;;      (set! nodes (cons (get-relative-node n1 6 i 6) nodes))
;;      (set-payload (first nodes) tile)
;;      (set! nodes (cons (get-relative-node n1 8 i 8) nodes))
;;      (set-payload (first nodes) tile)
;;      )
;;Fix: Order is wrong.
(define side-node (get-relative-node (get-parent n1) 0 -1 0))

(set-payload side-node tile2)
(newline)
(get-parent-offset n1 1 (get-parent n1))
(render-node (find-common-parent  side-node n1)
	      4
	      (lambda (node x y z s)
		(unless (null? (get-payload node))
			(printf "~a ~a ~a ~a\n" x y z s)))
	      0 0 0)
;(exit)
;(define side-node2 (get-relative-node n1 0 -1 0))
;(set-payload side-node2 tile)

 
(new my-canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
	(send dc set-brush (send the-brush-list find-or-create-brush 
				 (make-object color% 0 0 0 0.2) 'transparent))

	(let ((p (get-parent (get-parent (get-parent (get-parent (get-parent (get-parent n1))))))))
	  (let-values ([(px py pz) (get-parent-offset n1 24 p)])
	    (let-values ([(ox oy) (iso-offset px py pz)])
	      (render-node p (* 24 2 2 2 2 2 2)
			   (lambda (node x y z s) 
			     (let-values ([(nx ny) (iso-offset (+ x px) (+ y py) (+ z pz))])
			       (let ((tile (get-payload node)))
				 (unless (null? tile)
					 (printf "(~a ~a ~a ~a ~a)\n" x y z s (sprite-y tile))
					 (send dc draw-bitmap (sprite-image tile) 
					       (+ nx (sprite-x tile) 200) (+ ny (sprite-y tile) 200))
					 ;(send dc draw-rectangle (+ nx 200)
					 ;      (+ ny  200) s s)
					 ))))
			 0 0 0)
	    )))
	;(send dc draw-text "Dont panic" 0 0)
	)])




(send frame show #t)
