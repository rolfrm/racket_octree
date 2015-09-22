#lang racket

; functions:
; get-child-node (node idx) 
;  gets or creates a child node for idx.
; has-child-node (node idx) 
;  returns #t if the given node has a child node at idx.
; get-parent (node)
;  returns (or creates) the parent of a node.
; get-payload (node)
;  returns the payload of a node
; set-payload (node payload)
;  sets the payload of a node
; create-node


;; weak octree implementation.
;; As long as an object holds on to a 
;; node that tree branch exists.
;; Once the objects does not exists that node ceases to exist.
;; This is kind of beautiful. (abuse?)
(struct node ((sub-nodes ) (parent) (payload)) #:mutable)

(provide (contract-out
	 [get-child-node (-> node? integer? node?)]
	 [has-child-node (-> node? integer? boolean?)]
	 [get-parent-node (-> node? node?)]	    
	 [has-parent-node (-> node? boolean?)]
	 [get-node-payload (-> node? any/c)]
	 [set-node-payload! (-> node? any/c void)]
	 [create-node (->* ()(any/c) node?)]
	 ))

(define (create-node [payload null]) 
  (node null null payload))

(define get-node-payload node-payload)
(define (get-parent-node item)
  (if (null? (node-parent item))
      (let ((new-parent (node (make-vector 8 null) null null)))
	(vector-set! (node-sub-nodes new-parent) (* 7 (random 2)) (make-weak-box item))
	(set-node-parent! item new-parent)
	new-parent)
      (node-parent item)))

(define has-parent-node (compose not null? node-parent))

(define (get-child-node parent idx)
  (when (null? (node-sub-nodes parent))
	(set-node-sub-nodes! parent (make-vector 8 null)))
  (let ((current-value (vector-ref (node-sub-nodes parent) idx)))
    (if (or (null? current-value) (not (weak-box-value current-value)))
	(let ((new-value (node null parent null)))
	  (vector-set! (node-sub-nodes parent) idx (make-weak-box new-value))
	  new-value)
	(weak-box-value current-value))))

(define (has-child-node parent idx)
  (if (null? (node-sub-nodes parent))
      #f
      (not (null? (vector-ref (node-sub-nodes parent) idx)))))
