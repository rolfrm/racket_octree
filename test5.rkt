#lang racket/gui
(require racket/gui/base)
(define (parse-number s)
  (cond
   [(equal? "1" s) 1]
   [(equal? "2" s) 2]
   [else 0]))

(parse-number "1")

(require "octree-base.rkt")
(require "vec3.rkt")

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

(define (index-to-offset idx)
  (let ((lx (bitwise-bit-field idx 0 1))
	(ly (bitwise-bit-field idx 1 2))
	(lz (bitwise-bit-field idx 2 3)))
    (vec3 lx ly lz)))

(define (offset-to-index offset)
  (+ (vec3-x offset) (* (vec3-y offset) 2) (* (vec3-z offset) 4)))

(define (get-relative-node node v)
  (let ((idx (get-node-index node)))
    (let ((vi (index-to-offset idx))
	  (parent (get-parent node)))
      (let ((n (vec3-apply int-div (list (vec3+ vi v) (vec3 2 2 2)))))
	(let ((nparent (if (and (eq? (vec3-x n) 0) (eq? (vec3-y n) 0) (eq? (vec3-z n) 0))
			   parent
			   (get-relative-node parent n))))
	  (let ((l2 (vec3-apply bitwise-and (list (vec3+ n vi) (vec3 1 1 1)))))
	    (get-child-node nparent (offset-to-index l2))))))))


(define (render-node node size f (v (vec3 0 0 0)))
  (f node v size)
  (let ((s (/ size 2)))
    ;;it: xz: (1 0) (0 0) (1 1) (1 0)
    ;;        1     0     5     4
    ;;        3     2     7     6
    (for ([i '(1 0 5 4 3 2 7 6)])
	 (when (has-child-node node i)
	       (let* ((v2 (index-to-offset i))
		      (v3 (vec3+ v (vec3* v2 (vec3 s s s)))))
		 (render-node (get-child-node node i) s f v3)
		 )))))

;; Calculate the position relative to the parent node p
(define (get-parent-offset node s p)
  (let* ((idx (get-node-index node))
	 (parent (get-parent node))
	 (l (index-to-offset idx))
	 (r (vec3-apply (lambda (x y) (* x s -1)) (list l (vec3 0 0 0)))))
    (if (equal? parent p)
	r
	(let ((pv (get-parent-offset parent (* s 2) p)))
	  (vec3+ pv r)))))

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

(define (iso-offset o)
  (vec3 (+ (vec3-x o) (vec3-z o))
	(- (/ (vec3-z o) 2) (+ (vec3-y o) (/ (vec3-x o) 2)))
	0))

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
(define tile (sprite (read-bitmap "tile2.png") 0 -12));-37))
(define tile2 (sprite (read-bitmap "tile2x.png") 0 -24));-75));(sprite (read-bitmap "tree.png") 0 -14))
(define tile3 (sprite (read-bitmap "tile4x.png") 0 -48));-148))

; simple tag-object
; If its a game object it will have a local offset.
; If it has a visual it will have a sprite attached
(struct entity (position size))

;(define (to-parent-coords node coord)
  

;; (define (add-entity node position size)
;;   ;; Adds a new entity to the scene.
;;   ;; movies it to the appropiate level of detail.
;;   (if (ormap (lambda (x) (> x 1)) size)
;;       (add-entity (get-parent node) (to-parent-coords position) (to-parent-size size))
;;       (let ((new-node (apply relative-node node (map #'floor position))))
;; 	(set-payload node (cons (get-payload node) (entity (map (lambda (x) (- x (floor x))) position) size)))
;; 	new-node))
;;   )


(define p1 (create-node))
(define p2 (get-child-node p1 0))
(define p3 (get-child-node p1 4))

(set-payload p3 tile3)
(define p4 (get-child-node p2 0))
(set-payload p4 tile2)
(define p42 (get-child-node p2 4))
(set-payload p42 tile2)
(define p5 (get-child-node p2 0))
(define n1 (get-child-node p5 2))
(set-payload n1 tile) 

(render-node p1 1 
	     (lambda (node xyz s) (printf "~a\n" (list xyz s (get-payload node)))))

(define p32 (get-relative-node p3 (vec3 1 0 0)))
(set-payload p32 tile3)
(define p33 (get-relative-node p3 (vec3 1 0 -1)))
(set-payload p33 tile3)

(printf "~a\n" (iso-offset (vec3 1 2 3)))
(printf "~a\n" (get-parent-offset n1 24 p1))

;(exit)

(new my-canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
	(send dc set-brush (send the-brush-list find-or-create-brush 
				 (make-object color% 0 0 0 0.2) 'transparent))
	(let* ((p (get-parent (get-parent (get-parent (get-parent (get-parent (get-parent n1)))))))
	       (pv (get-parent-offset n1 24 p))
	       (o (iso-offset pv)))
	  (render-node p (* 24 2 2 2 2 2 2)
		       (lambda (node xyz s) 
			 (let ((im-v (iso-offset (vec3+ xyz pv)))
			       (tile (get-payload node)))
			   (unless (null? tile)
				   (send dc draw-bitmap (sprite-image tile) 
					 (+ (vec3-x im-v)  (sprite-x tile) 200) 
					 (+ (vec3-y im-v) (sprite-y tile) 200))
				   )))
	    )
	  ))
	;(send dc draw-text "Dont panic" 0 0)
	])




(send frame show #t)
