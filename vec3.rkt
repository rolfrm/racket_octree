#lang racket/base

(provide vec3 vec3-apply vec3-fold vec3+ vec3- vec3/ vec3* vec3-x vec3-y vec3-z vec3-values)

(struct vec3 (x y z) #:transparent)

(define (fold f args)
  (if (null? (cdr args))
      (car args)
      (f (car args) (fold f (cdr args)))))

(define (vec3-apply f . r)
  (vec3 (apply f (map vec3-x r))
	(apply f (map vec3-y r))
	(apply f (map vec3-z r))))

(define (vec3-fold f r)
  (vec3 (fold f (map vec3-x r))
	(fold f (map vec3-y r))
	(fold f (map vec3-z r))))

(define (vec3+ . a)
  (vec3-fold + a))

(define (vec3- . a)
  (vec3-fold - a))

(define (vec3* . a)
  (vec3-fold * a))

(define (vec3/ . a)
  (vec3-fold / a))

(define (vec3-values a)
  (list (vec3-x a) (vec3-y a) (vec3-z a)))
