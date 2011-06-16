;; Toy HTTP daemon
;;
;; Copyright (c) 2008, 2011 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;; Copyright (c) 2008 LShift Ltd. <query@lshift.net>
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

(define-record-type <servlet-index>
  (make-servlet-index* patterns)
  servlet-index?
  (patterns servlet-index-patterns set-servlet-index-patterns!))

(define-record-type <publication-pattern>
  (make-publication-pattern* template)
  publication-pattern?
  (template publication-pattern-template))

(define (make-httpd-servlet-handler current-request-parameter index)
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
      (parameterize ((current-request-parameter req))
	(let ((response (or (invoke-handler index req pieces)
			    (make-http-response 404 "Not Found" '((content-type "text/html"))
						(list
						 "<html><head>"
						 "<title>Page Not Found</title>"
						 "</head><body>"
						 "<h1>404: Page Not Found</h1>"
						 "<p>We can't find that page.</p>"
						 "</body></html>")))))
	  ;;(write (http-body->string (http-response-body response)))(newline)
	  response)))))

(define (make-servlet-index)
  (make-servlet-index* '()))

(define (reset-servlet-index! index)
  (set-servlet-index-patterns! index '()))

(define (publication-pattern-binding-element? element)
  (and (pair? element) (eq? (car element) 'unquote)))

(define (publication-pattern-binding-name binding) (cadr binding))

(define (parse-publication-pattern-template template)
  (map (lambda (element)
	 (cond
	  ((string? element) element)
	  ((symbol? element) (symbol->string element))
	  ((publication-pattern-binding-element? element) element)
	  (else (error `("Illegal pattern component in publish-pattern" ,element)))))
       template))

(define (make-publication-pattern template)
  (make-publication-pattern* (parse-publication-pattern-template template)))

(define (publish-pattern! index pattern handler)
  (when (not (publication-pattern? pattern))
    (error `("publish-pattern! expects a publication-pattern" ,pattern)))
  (set-servlet-index-patterns! index
			       (cons (cons pattern handler)
				     (servlet-index-patterns index))))

(define (invoke-handler index request value)
  (let search ((patterns (servlet-index-patterns index)))
    (if (null? patterns)
	#f
	(let ((bindings (match-publication-pattern (publication-pattern-template (caar patterns))
						   value)))
	  (if bindings
	      ((cdar patterns) request bindings)
	      (search (cdr patterns)))))))

(define (match-publication-pattern template value)
  (if (or (null? template) (null? value))
      (and (null? template) (null? value) '())
      (if (publication-pattern-binding-element? (car template))
	  (and-let* ((tail-bindings (match-publication-pattern (cdr template) (cdr value))))
	    (cons (list (publication-pattern-binding-name (car template))
			(car value))
		  tail-bindings))
	  (and (equal? (car template) (car value))
	       (match-publication-pattern (cdr template) (cdr value))))))

(define (instantiate-publication-pattern pattern bindings)
  (let* ((template (publication-pattern-template pattern))
	 (parts (map (lambda (element)
		       (cond ((string? element) element)
			     ((symbol? element) (symbol->string element))
			     ((publication-pattern-binding-element? element)
			      (cond
			       ((assq (publication-pattern-binding-name element)
				      bindings) => cadr)
			       (else (error `("Missing binding in instantiation"
					      ,template
					      ,element
					      ,bindings)))))))
		     template)))
    (string-concatenate (cons "/" (interleave-element "/" parts)))))

(define-syntax define-publication-patterns
  (syntax-rules ()
    ((_ (name template) ...)
     (begin (define name (make-publication-pattern 'template)) ...))))

(define (extract-publication-pattern-binding name bindings)
  (cond
   ((assq name bindings) => cadr)
   (else (error `("Missing binding"
		  ,name
		  ,bindings)))))

(define-syntax binding-let
  (syntax-rules ()
    ((_ bindings (name ...) body ...)
     (let ((temp bindings))
       (let ((name (extract-publication-pattern-binding 'name temp)) ...)
	 body ...)))))

(define-syntax publication-pattern-handler
  (syntax-rules ()
    ((_ (name ...) body ...)
     (lambda (request bindings)
       (binding-let bindings (name ...) body ...)))))

(define-syntax publish-pattern
  (syntax-rules ()
    ((_ index pattern (name ...) body ...)
     (publish-pattern! index
		       pattern
		       (publication-pattern-handler (name ...) body ...)))))
