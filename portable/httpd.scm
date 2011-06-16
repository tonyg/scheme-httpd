;; Toy HTTP daemon
;;
;; Copyright (c) 2008, 2011 Tony Garnock-Jones <tonygarnockjones@gmail.com>
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

(define-record-type <http-daemon>
  (make-http-daemon* port-number server-socket handler)
  http-daemon?
  (port-number http-daemon-port-number)
  (server-socket http-daemon-server-socket set-http-daemon-server-socket!)
  (handler http-daemon-handler))

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
      (let ((ch (read-byte i)))
	(cond
	 ((eof-object? ch) (list->string (reverse acc)))
	 ((= ch 13)
	  (if (not (= (read-byte i) 10))
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
  (let ((v (make-byte-vector len 0)))
    (if (not (= (read-block v 0 len i) len))
	'short-body
	v)))

(define (make-redirect-response uri)
  (make-http-response 307 "Temporary Redirect" `(("Location" ,uri))
		      (list "<a href=\""uri"\">"uri"</a>")))

(define write-crlf
  (lambda (o)
    (display "\r\n" o)))

(define write-status-line
  (lambda (o version code message)
    (display version o)
    (display #\space o)
    (display (number->string code) o)
    (display #\space o)
    (display message o)
    (write-crlf o)))

(define write-header
  (lambda (o key value)
    (display key o)
    (display #\: o)
    (display value o)
    (write-crlf o)))

(define (do-connection i o handler)
  (let* ((req (read-http-request i))
	 (content-length (and-let* ((s (assq 'content-length (http-request-headers req))))
			   (string->number (cadr s)))))
    (when content-length
      (set-http-request-body! req (read-body i content-length)))
    (let ((reply (handler req)))
      (write-status-line o
			 (or (http-request-http-version req) "HTTP/1.0")
			 (http-response-code reply)
			 (http-response-message reply))
      (for-each (lambda (header) (write-header o (car header) (cadr header)))
		(http-response-headers reply))
      (let ((body (flatten-iolist (http-response-body reply))))
	(if body
	    (let ((len (byte-vector-length body)))
	      (write-header o 'content-length (number->string len))
	      (write-crlf o)
	      (write-block body 0 len o))
	    (write-crlf 0))))
    (close-input-port i)
    (close-output-port o)))

(define (dump-error-to exn port)
  (pretty-print `(exception (exn ,exn)) port 0)
  (newline port))

(define (dump-error-and thunk)
  (lambda (exn)
    (dump-error-to exn (current-output-port))
    (thunk)))

(define (complain-and-close exn i o)
  (lambda ()
    (display "HTTP/1.0 500 Internal Server Error" o) (write-crlf o)
    (display "Content-type: text/plain" o) (write-crlf o)
    (write-crlf o)
    (display "Internal Server Error" o) (write-crlf o)
    (write-crlf o)
    (dump-error-to exn o)
    (close-input-port i)
    (close-output-port o)))

(define (on-exception handler thunk)
  (call-with-current-continuation
   (lambda (escape)
     (with-exception-handler
      (lambda (exn)
	(escape (handler exn)))
      thunk))))

(define (http-daemon-running? d)
  (not (eq? #f (http-daemon-server-socket d))))

(define (run-http-daemon d)
  (when (http-daemon-running? d)
    (error "Cannot run already-running daemon"))
  (let ((server-socket (open-socket (http-daemon-port-number d))))
    (set-http-daemon-server-socket! d server-socket)
    (let accept-loop ()
      (call-with-current-continuation
       (lambda (escape)
	 (call-with-values (lambda ()
			     (on-exception
			      (lambda (exn)
				(set-http-daemon-server-socket! d #f)
				(escape #t))
			      (lambda () (socket-accept server-socket))))
	   (lambda (in out)
	     (spawn
	      (lambda ()
		(on-exception
		 (lambda (exn)
		   ((dump-error-and (complain-and-close exn in out)) exn))
		 (lambda ()
		   (do-connection in
				  out
				  (http-daemon-handler d))))))
	     (accept-loop))))))))

(define (spawn-http-daemon d)
  (spawn
   (lambda ()
     (on-exception
      (lambda (exn)
	((dump-error-and (lambda ()
			   (on-exception (lambda (exn) 'ignore)
					 (lambda () (stop-http-daemon d)))))
	 exn))
      (lambda ()
	(run-http-daemon d))))))

(define (stop-http-daemon d)
  (let ((s (http-daemon-server-socket d)))
    (when s
      (close-socket s))
    #t))

(define (make-http-daemon port-number handler . maybe-and-start?)
  (let* ((and-start? (and (pair? maybe-and-start?) (car maybe-and-start?)))
	 (d (make-http-daemon* port-number #f handler)))
    (when and-start?
      (spawn-http-daemon d))
    d))
