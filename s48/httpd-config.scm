(define-structure when
  (export (when :syntax))
  (open scheme)
  (begin (define-syntax when
	   (syntax-rules ()
	     ((_ test body ...)
	      (if test
		  (begin body ...)))))))

(define-structure httpd-utils
  (export string-split
	  flatten-iolist
	  interleave-element
	  unquote-http-url)
  (open scheme)
  (open srfi-2)
  (open srfi-13)
  (open srfi-23)
  (open encodings)
  (files "../portable/httpd-utils.scm"))

(define-structure httpd
  (export make-http-daemon
	  http-daemon?
	  http-daemon-port-number
	  http-daemon-running?
	  stop-http-daemon
	  run-http-daemon
	  spawn-http-daemon

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
  (open scheme)

  (open srfi-1)
  (open srfi-2)
  (open srfi-9)
  (open srfi-13)
  (open srfi-23)
  (open srfi-34)

  (open sockets)
  (open threads)
  (open pp)
  (open i/o)
  (open byte-vectors)
  (open encodings)

  (open when)
  (open httpd-utils)

  (files "../portable/httpd.scm"))

(define-structure httpd-servlet
  (export make-httpd-servlet-handler

	  make-servlet-index
	  servlet-index?
	  reset-servlet-index!
	  publish-pattern!
	  invoke-handler

	  make-publication-pattern
	  instantiate-publication-pattern
	  extract-publication-pattern-binding

	  (define-publication-patterns :syntax)
	  (binding-let :syntax)
	  (publication-pattern-handler :syntax)
	  (publish-pattern :syntax))
  (open scheme)

  (open srfi-2)
  (open srfi-9)
  (open srfi-13)
  (open srfi-23)
  (open srfi-39)

  (open pp)

  (open when)
  (open httpd)
  (open httpd-utils)

  (files "../portable/httpd-servlet.scm"))
