#lang racket/base

(provide vec3 vec3-apply vec3+ vec3- vec3/ vec3* vec3-x vec3-y vec3-z)

(struct vec3 (x y z) #:transparent)

(define (fold f args)
  (if (null? (cdr args))
      (car args)
      (f (car args) (fold f (cdr args)))))

(define (vec3-apply f r)
  (vec3 (fold f (map vec3-x r))
	(fold f (map vec3-y r))
	(fold f (map vec3-z r))))

(define (vec3+ . a)
  (vec3-apply + a))

(define (vec3- . a)
  (vec3-apply - a))

(define (vec3* . a)
  (vec3-apply * a))

(define (vec3/ . a)
  (vec3-apply / a))



