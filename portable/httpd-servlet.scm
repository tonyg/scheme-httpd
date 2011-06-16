;; Toy HTTP daemon
;;
;; Copyright (c) 2011 Tony Garnock-Jones <tonygarnockjones@gmail.com>
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-syntax dispatch-rules
  (syntax-rules ()
    ((_ (pattern function) ...)
     (let ((table (list (list 'pattern function) ...)))
       (values (dispatch-case* table)
	       (dispatch-url* table))))))

(define-syntax dispatch-case
  (syntax-rules ()
    ((_ (pattern function) ...)
     (let ((table (list (list 'pattern function) ...)))
       (dispatch-case* table)))))

(define-syntax dispatch-url
  (syntax-rules ()
    ((_ (pattern function) ...)
     (let ((table (list (list 'pattern function) ...)))
       (dispatch-url* table)))))

(define (dispatch-case* table)
  (lambda (req)
    (let* ((path (http-request-parsed-path req))
	   (headers (http-request-headers req))
	   (method (http-request-method req))
	   (pieces (parsed-path-pieces path)))
      (pretty-print `((query ,(parsed-path-query path))
		      (pieces ,(parsed-path-pieces path))
		      (headers ,headers)
		      (method ,method))
		    (current-output-port)
		    0)
      (newline)
      (let ((response (or (invoke-handler table req pieces)
			  (make-http-response 404 "Not Found" '((content-type "text/html"))
					      (list
					       "<html><head>"
					       "<title>Page Not Found</title>"
					       "</head><body>"
					       "<h1>404: Page Not Found</h1>"
					       "<p>We can't find that page.</p>"
					       "</body></html>")))))
	;;(write (http-body->string (http-response-body response)))(newline)
	response))))

(define (dispatch-url* table)
  (lambda (proc . bindings)
    (let ((pattern (find (lambda (cell) (eq? (cadr cell) proc)) table)))
      (when (not pattern)
	(error `("Cannot find URL corresponding to procedure" ,proc)))
      (string-concatenate
       (cons "/" (interleave-element "/" (render-pattern (car pattern) bindings)))))))

(define (invoke-handler patterns request pieces)
  (let search ((patterns patterns))
    (if (null? patterns)
	#f
	(let ((bindings (match-pattern (caar patterns) pieces)))
	  (if bindings
	      (apply (cadar patterns) request bindings)
	      (search (cdr patterns)))))))

(define (pattern-starts-with-repeated-piece? pattern)
  (and (pair? (cdr pattern))
       (eq? (cadr pattern) '...)))

(define (match-pattern pattern pieces)
  (let loop ((bindings '())
	     (pattern pattern)
	     (pieces pieces))
    (cond
     ((and (null? pattern) (null? pieces)) (reverse bindings))
     ((or  (null? pattern) (null? pieces)) #f)
     ((equal? (car pattern) "") ;; skip empty pieces, for compatibility (?) with racket
      (loop bindings (cdr pattern) pieces))
     ((pattern-starts-with-repeated-piece? pattern)
      (let accumulate-list ((vals '()) (pieces pieces))
	(match1 vals (car pattern) (car pieces)
		(lambda (new-vals) (accumulate-list new-vals (cdr pieces)))
		(lambda () (loop (cons (reverse vals) bindings) (cddr pattern) pieces)))))
     (else
      (match1 bindings (car pattern) (car pieces)
	      (lambda (new-bindings) (loop new-bindings (cdr pattern) (cdr pieces)))
	      (lambda () #f))))))

(define (match-number str fk sk)
  (let ((v (string->number str)))
    (if v
	(sk v)
	(fk))))

(define (match1 bindings pat val sk fk)
  (cond
   ((equal? pat val) (sk bindings))
   ((equal? pat '(string-arg)) (sk (cons val bindings)))
   ((equal? pat '(symbol-arg)) (sk (cons (string->symbol val) bindings)))
   ((equal? pat '(number-arg))
    (match-number val fk (lambda (v) (sk (cons v bindings)))))
   ((equal? pat '(real-arg))
    (match-number val fk (lambda (v) (sk (cons (real-part v) bindings)))))
   ((equal? pat '(integer-arg))
    (match-number val fk (lambda (v)
			   (sk (cons (inexact->exact (truncate (real-part v))) bindings)))))
   ((string? pat) (fk))
   (else (error `("Invalid dispatch-pattern piece" ,pat)))))

(define (render-pattern pattern0 bindings0)
  (let loop ((pieces '())
	     (pattern pattern0)
	     (bindings bindings0))
    (cond
     ((null? pattern)
      (if (null? bindings)
	  (reverse pieces)
	  (error `("Too many arguments to pattern" ,pattern0 ,bindings0))))
     ((equal? (car pattern) "") ;; skip empty pieces, as above
      (loop pieces (cdr pattern) bindings))
     ((pattern-starts-with-repeated-piece? pattern)
      (when (not (list? (car bindings)))
	(error `("Expected repeated value" ,(car pattern) ,(car bindings))))
      (let flatten ((vals (car bindings)) (pieces pieces))
	(if (null? vals)
	    (loop pieces (cddr pattern) (cdr bindings))
	    (render1 pieces (car pattern) vals
		     (lambda (new-pieces new-vals) (flatten new-vals new-pieces))
		     (lambda () (error `("Could not render" ,pattern0 ,bindings0)))))))
     (else
      (render1 pieces (car pattern) bindings
	       (lambda (new-pieces new-bindings) (loop new-pieces (cdr pattern) new-bindings))
	       (lambda () (error `("Could not render" ,pattern0 ,bindings0))))))))

(define (render1 pieces pat bindings sk fk)
  (if (string? pat)
      (sk (cons pat pieces) bindings)
      (let ((val (car bindings))
	    (proceed (lambda (new-piece)
		       (sk (cons new-piece pieces) (cdr bindings)))))
	(cond
	 ((equal? pat '(string-arg)) (proceed val))
	 ((equal? pat '(symbol-arg)) (proceed (symbol->string val)))
	 ((equal? pat '(number-arg)) (proceed (number->string val)))
	 ((equal? pat '(real-arg)) (proceed (number->string (real-part val))))
	 ((equal? pat '(integer-arg))
	  (proceed (number->string (inexact->exact (truncate (real-part val))))))
	 (else (error `("Invalid dispatch-pattern piece" ,pat)))))))

(define (extract-binding name bindings)
  (cond
   ((assq name bindings) => cadr)
   (else (error `("Missing binding"
		  ,name
		  ,bindings)))))

(define-syntax binding-let
  (syntax-rules ()
    ((_ bindings (name ...) body ...)
     (let ((temp bindings))
       (let ((name (extract-binding 'name temp)) ...)
	 body ...)))))
