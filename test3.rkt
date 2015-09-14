#lang racket/gui

(require racket/gui/base)

(define frame (new frame% [label "Example"]))

(define msg (new message% [parent frame]
                          [label "No events so far..."]))
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])
 
; Show the frame by calling its show method
;

(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
; Make a canvas that handles events in the frame

(define bmap  (make-object bitmap% 100 100))

(print "----")
(display (send bmap load-file "hello.png"))
(newline)

(define dc2 (send bmap make-dc))
(define b1 (send the-brush-list find-or-create-brush (make-object color% 255 255 255 0.1)	 
 	 	'solid))
(define b2 (send the-brush-list find-or-create-brush (make-object color% 0 0 0 0)	 
 	 	'solid))
(send dc2 set-brush b1)
(send dc2 draw-rectangle 0 0 100 100)
(send dc2 set-brush b2)
(for ([i (in-range 100)])
     (send dc2 draw-arc (random 100) (random 100) 10 10 0 (* 3.14 2.0 (random))))
(send bmap save-file "hello.png" 'png)
(new my-canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
	(send canvas set-canvas-background (make-object color% 255 0 0 0))
	(send dc set-scale 3 3)
	(send dc set-text-foreground "blue")
	(send dc draw-text "Dont panic" 0 0))])

(new button% [parent frame]
             [label "Pause"]
             [callback (lambda (button event) (sleep 5))])

(define panel (new horizontal-panel% [parent frame]))
(new button% [parent panel]
             [label "Left"]
             [callback (lambda (button event)
                         (send msg set-label "Left click"))])
(new button% [parent panel]
             [label "Right"]
             [callback (lambda (button event)
                         (send msg set-label "Right click"))])
(send frame show #t)
