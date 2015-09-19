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
  (vec3 (bitwise-bit-field idx 0 1)
	(bitwise-bit-field idx 1 2)
	(bitwise-bit-field idx 2 3)))

(define (offset-to-index offset)
  (+ (vec3-x offset) 
     (* (vec3-y offset) 2) 
     (* (vec3-z offset) 4)))

(define (get-relative-node node v)
  (let ((idx (get-node-index node)))
    (let ((vi (index-to-offset idx))
	  (parent (get-parent node)))
      (let ((n (vec3-fold int-div (list (vec3+ vi v) (vec3 2 2 2)))))
	(let ((nparent (if (and (eq? (vec3-x n) 0) 
				(eq? (vec3-y n) 0) 
				(eq? (vec3-z n) 0))
			   parent
			   (get-relative-node parent n))))
	  (let ((l2 (vec3-fold bitwise-and (list (vec3+ v vi) (vec3 1 1 1)))))
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
  (if (eq? node p)
      (vec3 0 0 0)
      (let* ((idx (get-node-index node))
	     (parent (get-parent node))
	     (l (index-to-offset idx))
	     (r (vec3-apply (lambda (x) (* x s -1)) l)))
	(if (equal? parent p)
	    r
	    (let ((pv (get-parent-offset parent (* s 2) p)))
	      (vec3+ pv r))))))

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
(define (to-parent-coords2 idx coords)
  (let ((offset (index-to-offset idx)))
    (vec3-apply (lambda (x) (/ x 2))  (vec3+ offset coords))))

(define (to-parent-coords node coords)
  (to-parent-coords2 (get-node-index node) coords))

(define (to-parent-size size)
  (vec3/ size (vec3 2 2 2)))

(define (to-child-size size)
  (vec3* size (vec3 2 2 2)))

(define (to-child-coords2 offset coords)
  (vec3- (vec3* coords (vec3 2 2 2)) offset))

(define (lookup-blocks node position size cb (idx -1))
  ;; Finds all blocks that collides with block
  ;; and contains a payload
  ;; idx == -2 means its going down.
  ;; idx == -1 means it has just started.
  ;; This is to avoid it iterating between child and parent.
  (let ((stop (vec3+ position size)))
    (let ((collision (and (< (vec3-x position) 1)
			  (< (vec3-y position) 1)
			  (< (vec3-z position) 1)
			  (> (vec3-x stop) 0)
			  (> (vec3-y stop) 0)
			  (> (vec3-z stop) 0))))

      (when collision
	    (cb node position size)
	    (for ([i (in-range 8)]
		  #:when (and (not (eq? i idx)) 
			      (has-child-node node i))
		  )
		   (let ((offset (index-to-offset i)))
		     (lookup-blocks (get-child-node node i) 
				    (to-child-coords2 offset position) 
				    (to-child-size size) cb -2))))))
  (when (and (not (eq? idx -2)) 
	     (has-parent node))
	(let ((idx2 (get-node-index node)))
	  (lookup-blocks (get-parent node) (to-parent-coords2 idx2 position) 
			 (to-parent-size size) cb idx2)
	  )))
			     
		     
		 
		 

;; Base item size should never change as this will decrease visual quality.
;; Since its isometric there is a mismatch between the way stuff is understood
;; and the way it is rendered.
;; To fix this, an offset can be attached to each tile sprite.
;; That offset can be used for sprites that are smaller or larger than their designated cube.
;; Note that most things are bigger than their designated cubes

(define (iso-offset o)
  (vec3 (+ (vec3-x o) (vec3-z o))
	(- (/ (vec3-z o) 2) (vec3-y o) (/ (vec3-x o) 2))
	0))

(struct sprite (image x y))
(define tile (sprite (read-bitmap "tile2.png") 0 -37));-37))
(define tile2 (sprite (read-bitmap "tile2x.png") 0 -75));-75));(sprite (read-bitmap "tree.png") 0 -14))
(define tile3 (sprite (read-bitmap "tile4x.png") 0 -150));-148))
(define horsie (sprite (read-bitmap "horsie.png") 0 -25))

; simple tag-object
; If its a game object it will have a local offset.
; If it has a visual it will have a sprite attached
(struct entity (position size node) #:mutable)


(define (add-entity node entity)
  ;; Adds a new entity to the scene.
  ;; movies it to the appropiate level of detail.
  (let* ((size (entity-size entity))
	 (position (entity-position entity))
	 (size-values (vec3-values size)))
    (if (ormap (lambda (x) (> x 1)) size-values)
	(begin
	  (set-entity-position! entity (to-parent-coords node position))
	  (set-entity-size! entity (to-parent-size size))
	  (add-entity (get-parent node) entity))
      (if (andmap (lambda (x) (<= x 1/2)) size-values)
	  (begin
	    (set-entity-position! entity (vec3* position (vec3 2 2 2)))
	    (set-entity-size! entity (vec3* size (vec3 2 2 2)))
	    (add-entity (get-child-node node 0) entity))
	  (let ((new-node (get-relative-node node (vec3-apply exact-floor position))))
	    (set-entity-position! entity (vec3-apply (lambda (x) (- x (floor x))) position))
	    (set-entity-node! entity new-node)
	    (set-payload new-node (cons entity (get-payload new-node)))
	    entity)))))

(define (remove-entity entity)
  (let ((node (entity-node entity)))
    (set-payload node (remove entity (get-payload node)))
    (set-entity-node! entity null)))

(define p1 (create-node))
;; (get-relative-node p1 (vec3-apply exact-floor (vec3 0.1 0 0)))
;; (define l2 (add-entity p1 (vec3 0.1 0 0) (vec3 0.4 0.4 0.4)))
;; (add-entity p1 (vec3 0.2 0 0) (vec3 0.2 0.2 0.2))
;; (define c (find-common-parent (entity-node l2) p1))
;; (printf "C: ~a\n" c)
;; (printf "~a\n" (map (lambda (x) (eq? c x)) (get-parent-tree (entity-node l2))))
;; (printf "~a\n" (map (lambda (x) (eq? c x)) (get-parent-tree p1)))
;; (printf "eq: ~a\n" (eq? c  l2))
;; (print (get-parent-offset (entity-node l2) 1 c))
;; (newline)
;; (printf "Payload: ~a" (get-payload (entity-node l2)))
;(exit 0)

(define p2 (get-child-node p1 0))
(define p3 (get-child-node p1 4))

(set-payload p3 tile3)
(define p4 (get-child-node p2 0))
(set-payload p4 tile2)
(define p42 (get-child-node p2 4))
(set-payload p42 tile2)
(define p5 (get-child-node p2 0))
(define n1 (get-child-node p5 2))
;(set-payload n1 (list tile)) 

(define sprite-table (make-weak-hash))

(render-node p1 1 (lambda (node xyz s) (printf "~a\n" (list xyz s (get-payload node)))))

(define p32 (get-relative-node p3 (vec3 1 0 0)))
;(set-payload p32 tile3)
(define p33 (get-relative-node p3 (vec3 1 0 -1)))
;(set-payload p33 tile3)
(define p34 (get-relative-node p3 (vec3 1 0 -2)))
;(set-payload p34 tile3)
(define e1 (add-entity n1 (entity (vec3 0 5 1) (vec3 1 1 1) null)))
(hash-set! sprite-table e1 horsie)

;(printf "~a\n" (iso-offset (vec3 1 2 3)))
;(printf "~a\n" (get-parent-offset n1 24 p1))

;(exit)
(define frame (new frame% [label "Example"] [width 512] [height 512]))  
;(define msg (new message% [parent frame]))

(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
 ;   (define/override (on-event event)
 ;     (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
    (define dir (vec3 0 0 0))
    (define/public (get-dir) dir)
      
    (define/override (on-char event)
      (case (send event get-key-code)
	['up (set! dir   (vec3 1 0 0))]
	['down (set! dir (vec3 -1 0 0))]
	['left (set! dir (vec3 0 0 -1))]
	['right (set! dir (vec3 0 0 1))]
	['#\w (set! dir (vec3 0 1 0))]
	['#\s (set! dir (vec3 0 -1 0))]
	[else (set! dir (vec3 0 0 0))])
      (when (char? (send event get-key-code))
	    (printf "~a\n" (char->integer (send event get-key-code))))
      (send this refresh-now)
      ;(send msg set-label "Canvas keyboard")
      )
    ; Call the superclass init, passing on all init args
    (super-new)))

(new my-canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
	(send dc set-brush (send the-brush-list find-or-create-brush
				 (make-object color% 0 0 0 1) 'solid))
	(send canvas set-canvas-background (make-object color% 0 0 0 1))
	(let ((node (entity-node e1)))
	  (remove-entity e1)	  
	  (let ((ep (entity-position e1)))
	    (let ((np (vec3+ ep (vec3-apply (lambda (x) (/ x 10)) (send canvas  get-dir))))
		  (collides false))
	      (lookup-blocks node np (entity-size e1) 
			   (lambda (n p s) 
			     (unless 
			      (null? (get-payload n)) 
			      (set! collides true)
			      ;(set-payload n null)
			      (printf "Collision with: ~a ~a ~a ~a\n" (get-payload n) p s ep)
			      )))
	  
	      (unless collides

		      (set-entity-position! e1 np)

		      )
	      (add-entity node e1)
	    ;; Apply gravity. First check if there is a block below.
	    ;(let ((gp (vec3- ep (vec3 0 0.1 0))))
	    ;  (for ([blk (lookup-blocks node gp (entity-size))]
	      (newline)
	    
	  
	      
	      )))
	

	(let* ((p (get-parent (get-parent (get-parent (get-parent (get-parent (get-parent (entity-node e1))))))))
	       (pv (get-parent-offset (entity-node e1) 24 p))
	       (o (iso-offset pv)))
	  (render-node p (* 24 2 2 2 2 2 2)
		       (lambda (node xyz s)
			 (let ((im-v (iso-offset (vec3+ xyz pv (vec3-apply (lambda (x) (* x 24 -1)) (entity-position e1)))))
			       (tile (get-payload node)))
			   (unless (null? tile)
				   (let ((lst (if (pair? tile) tile (cons tile null))))
				     (for ([i lst])
					  (when (sprite? i)
						(send dc draw-bitmap (sprite-image i)
						      (+ (vec3-x im-v) (sprite-x i) 200)
						      (+ (vec3-y im-v) (sprite-y i) 200)))
					  (when (entity? i)
						(let ((sp (hash-ref sprite-table e1 horsie)))
						  (unless (null? sp)
							  (let ((offset (vec3+ im-v (iso-offset (vec3-apply (lambda (x) (* x s)) (entity-position i))))))
							    (send dc draw-bitmap (sprite-image sp)
								  (+ (vec3-x offset) (sprite-x sp) 200)
								  (+ (vec3-y offset) (sprite-y sp) 200)))
							  (printf "Entity! ~a ~a\n" (entity-size i) (entity-position i)))))
				   )))))
	    )
	  ))
	;(send dc draw-text "Dont panic" 0 0)
	])




(send frame show #t)
