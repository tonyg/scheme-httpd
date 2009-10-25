;; Toy HTTP daemon
;;
;; Copyright (c) 2008 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
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

(require-library 'sisc/libs/srfi/srfi-1)
(require-library 'sisc/libs/srfi/srfi-2)
(require-library 'sisc/libs/srfi/srfi-9)
(require-library 'sisc/libs/srfi/srfi-13)

(module httpd-utils
    (string-split)

  (define string-split
    (let ()
      (define (finish-part part parts)
	(if (null? part)
	    parts
	    (cons (list->string (reverse part)) parts)))

      (lambda (str separators . maybe-max-split)
	(define (split-loop-counting remaining parts part chars)
	  (if (null? chars)
	      (reverse (finish-part part parts))
	      (if (memv (car chars) separators)
		  (let ((new-remaining (- remaining 1)))
		    (if (zero? new-remaining)
			(reverse (cons (list->string (cdr chars))
				       (finish-part part parts)))
			(split-loop-counting new-remaining
					     (finish-part part parts) '() (cdr chars))))
		  (split-loop-counting remaining parts (cons (car chars) part) (cdr chars)))))
	(define (split-loop-not-counting parts part chars)
	  (if (null? chars)
	      (reverse (finish-part part parts))
	      (if (memv (car chars) separators)
		  (split-loop-not-counting (finish-part part parts) '() (cdr chars))
		  (split-loop-not-counting parts (cons (car chars) part) (cdr chars)))))
	(let ((initial-max-split (and (pair? maybe-max-split) (car maybe-max-split))))
	  (if (not (or (not initial-max-split)
		       (and (integer? initial-max-split)
			    (not (negative? initial-max-split)))))
	      (error `(bad-max-split ,initial-max-split))
	      (if (integer? initial-max-split)
		  (if (zero? initial-max-split)
		      (list str)
		      (split-loop-counting initial-max-split '() '() (string->list str)))
		  (split-loop-not-counting '() '() (string->list str))))))))
)

(module httpd
    (httpd
     stop-httpd

     http-body->string
     string->http-body

     make-http-request
     http-request?
     http-request-headers
     http-request-method
     http-request-path
     http-request-parsed-path
     http-request-http-version
     http-request-body

     make-parsed-path
     parsed-path?
     parsed-path-query-string
     parsed-path-query
     parsed-path-params
     parsed-path-path-string
     parsed-path-pieces

     make-redirect-response

     make-http-response
     http-response?
     http-response-code
     http-response-message
     http-response-headers
     http-response-body)

  (import s2j)
  (import threading)
  (import srfi-1)
  (import srfi-2)
  (import srfi-9)
  (import srfi-13)

  (import httpd-utils)

  (define-record-type <http-request>
    (make-http-request headers method path parsed-path http-version body)
    http-request?
    (headers http-request-headers set-http-request-headers!)
    (method http-request-method set-http-request-method!)
    (path http-request-path set-http-request-path!)
    (parsed-path http-request-parsed-path set-http-request-parsed-path!)
    (http-version http-request-http-version set-http-request-http-version!)
    (body http-request-body set-http-request-body!))

  (define-record-type <parsed-path>
    (make-parsed-path query-string query params path-string pieces)
    parsed-path?
    (query-string parsed-path-query-string set-parsed-path-query-string!)
    (query parsed-path-query set-parsed-path-query!)
    (params parsed-path-params set-parsed-path-params!)
    (path-string parsed-path-path-string set-parsed-path-path-string!)
    (pieces parsed-path-pieces set-parsed-path-pieces!))

  (define-record-type <http-response>
    (make-http-response code message headers body)
    http-response?
    (code http-response-code set-http-response-code!)
    (message http-response-message set-http-response-message!)
    (headers http-response-headers set-http-response-headers!)
    (body http-response-body set-http-response-body!))

  (define-java-classes
    <java.io.buffered-input-stream>
    <java.io.data-output-stream>
    <java.io.print-writer>
    <java.lang.string>
    <java.net.server-socket>
    )

  (define-generic-java-methods
    accept
    get-input-stream
    get-output-stream
    (jlength length)
    close
    get-bytes
    write-bytes
    write-byte
    (jwrite write)
    (jread read)
    )

  (define (http-body->string j)
    (and j (->string (java-new <java.lang.string> j))))

  (define (string->http-body s)
    (and s (get-bytes (->jstring s))))

  (define (alist-push alist key value)
    (call-with-values (lambda () (break (lambda (elt) (eq? (car elt) key)) alist))
      (lambda (head tail)
	(if (null? tail)
	    (cons (list key value) alist)
	    (cons (cons key (append (cdar tail) (list value))) (append head (cdr tail)))))))

  (define (parse-query query)
    (let ((kvs (string-split query '(#\&))))
      `(query ,@(fold (lambda (kv acc) (alist-push acc (car kv) (cadr kv)))
		      '()
		      (map (lambda (kv) (split-header-by '(#\=) kv)) kvs)))))

  (define (parse-path path)
    (apply (lambda (path-and-params . maybe-query)
	     (apply (lambda (path . maybe-params)
		      (let ((path-pieces (string-split path '(#\/)))
			    (query-string (and (pair? maybe-query) (car maybe-query))))
			(make-parsed-path query-string
					  (if query-string (parse-query query-string) '())
					  (if (null? maybe-params) #f (car maybe-params))
					  path
					  path-pieces)))
		    (string-split path-and-params '(#\;) 1)))
	   (string-split path '(#\?) 1)))

  (define (split-header-by separators line)
    (apply (lambda (key . maybe-value)
	     (list (string->symbol (string-downcase (string-trim-both key)))
		   (if (null? maybe-value)
		       #f
		       (string-trim-both (car maybe-value)))))
	   (string-split line separators 1)))

  (define (read-http-request i)
    (define (next-line)
      (let loop ((acc '()))
	(let ((ch (->number (jread i))))
	  (cond
	   ((= ch -1) (list->string (reverse acc)))
	   ((= ch 13)
	    (if (not (= (->number (jread i)) 10))
		(error "Invalid line terminator - please supply CRLFs")
		(list->string (reverse acc))))
	   (else
	    (loop (cons (integer->char ch) acc)))))))
    (define (parse-headers acc)
      (let ((line (next-line)))
	(if (zero? (string-length line))
	    (reverse acc)
	    (parse-headers (cons (split-header-by '(#\:) line) acc)))))
    (apply (lambda (method-str path . maybe-http-version)
	     (let ((method (string->symbol (string-downcase method-str)))
		   (headers (parse-headers '())))
	       (make-http-request headers
				  method
				  path
				  (parse-path path)
				  (if (null? maybe-http-version) #f (car maybe-http-version))
				  #f)))
	   (string-split (next-line) '(#\space) 2)))

  (define (read-body i len)
    (let ((v (java-array-new <jbyte> len)))
      (if (not (= (->number (jread i v)) len))
	  'short-body
	  v)))

  (define (make-redirect-response uri)
    (make-http-response 307 "Temporary Redirect" `(("Location" ,uri))
			(string->http-body
			 (string-append "<a href=\""uri"\">"uri"</a>"))))

  (define write-crlf
    (let ((cr (->jbyte (char->integer #\return)))
	  (lf (->jbyte (char->integer #\newline))))
      (lambda (o)
	(write-byte o cr)
	(write-byte o lf))))

  (define write-status-line
    (let ((sp (->jbyte (char->integer #\space))))
      (lambda (o version code message)
	(write-bytes o (->jstring version))
	(write-byte o sp)
	(write-bytes o (->jstring (number->string code)))
	(write-byte o sp)
	(write-bytes o (->jstring message))
	(write-crlf o))))

  (define write-header
    (let ((colon (->jbyte (char->integer #\:))))
      (lambda (o key value)
	(write-bytes o (->jstring key))
	(write-byte o colon)
	(write-bytes o (->jstring value))
	(write-crlf o))))

  (define (do-connection connection handler)
    (let* ((i (java-new <java.io.buffered-input-stream> (get-input-stream connection)))
	   (o (java-new <java.io.data-output-stream> (get-output-stream connection)))
	   (req (read-http-request i))
	   (content-length (and-let* ((s (assq 'content-length (http-request-headers req))))
			     (string->number (cadr s)))))
      (if content-length
	  (set-http-request-body! req (read-body i content-length)))
      (let ((reply (handler req)))
	(write-status-line o
			   (or (http-request-http-version req) "HTTP/1.0")
			   (http-response-code reply)
			   (http-response-message reply))
	(for-each (lambda (header) (write-header o (car header) (cadr header)))
		  (http-response-headers reply))
	(let ((body (http-response-body reply)))
	  (if body
	      (write-header o 'content-length (number->string (java-array-length body))))
	  (write-crlf o)
	  (if body
	      (jwrite o body))))
      (close connection)))

  (define (dump-error-to e m port)
    (import debugging)
    (parameterize ((current-output-port port))
      (pretty-print `(exception (e ,e) (m ,m)))
      (newline port)
      (print-stack-trace m)
      (newline port)))

  (define (dump-error-and thunk)
    (lambda (e m)
      (dump-error-to e m (current-output-port))
      (thunk)))

  (define (complain-and-close e m connection)
    (import java-io)
    (lambda ()
      (let* ((o (get-output-stream connection))
	     (pj (java-new <java.io.print-writer> o))
	     (p (->character-output-port pj #t)))
	(display "HTTP/1.0 500 Internal Server Error" p) (newline p)
	(display "Content-type: text/plain" p) (newline p)
	(newline p)
	(display "Internal Server Error" p) (newline p)
	(newline p)
	(dump-error-to e m p)
	(close connection))))

  (define (httpd port-number handler)
    (let ((server-socket (java-new <java.net.server-socket> (->jint port-number))))
      (define (accept-loop)
	(let ((connection (accept server-socket)))
	  (thread/start (thread/new (lambda ()
				      (with-failure-continuation
				       (lambda (e m)
					 ((dump-error-and (complain-and-close e m connection)) e m))
				       (lambda ()
					 (do-connection connection handler))))))
	  (accept-loop)))
      (thread/start (thread/new (lambda ()
				  (with-failure-continuation
				   (dump-error-and (lambda () (close server-socket)))
				   accept-loop))))
      (cons 'httpd server-socket)))

  (define (stop-httpd r)
    (if (not (eq? (car r) 'httpd))
	(error `("Cannot stop-httpd non-HTTPD" ,r))
	(close (cdr r)))
    #t)
)

(module httpd-servlet
    (make-httpd-servlet-handler

     make-servlet-index
     servlet-index?
     reset-servlet-index!
     publish-pattern!
     invoke-handler

     make-publication-pattern
     instantiate-publication-pattern
     extract-publication-pattern-binding

     (define-publication-patterns make-publication-pattern)
     (binding-let extract-publication-pattern-binding)
     publication-pattern-handler
     publish-pattern)

  (import srfi-2)
  (import srfi-9)

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
      (import httpd)
      (let* ((path (http-request-parsed-path req))
	     (headers (http-request-headers req))
	     (method (http-request-method req))
	     (pieces (parsed-path-pieces path)))
	(pretty-print `((query ,(parsed-path-query path))
			(pieces ,(parsed-path-pieces path))
			(headers ,headers)
			(method ,method)))
	(newline)
	(parameterize ((current-request-parameter req))
	  (let ((response (or (invoke-handler index req pieces)
			      (make-http-response 404 "Not Found" '((content-type "text/html"))
						  (string->http-body
						   (string-append
						    "<html><head>"
						    "<title>Page Not Found</title>"
						    "</head><body>"
						    "<h1>404: Page Not Found</h1>"
						    "<p>We can't find that page.</p>"
						    "</body></html>"))))))
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
    (if (not (publication-pattern? pattern))
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
)

;;; Local Variables:
;;; eval: (put 'publish-pattern 'scheme-indent-function 3)
;;; End:
