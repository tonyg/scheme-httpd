(define-structure httpd-utils
  (export string-split
	  flatten-iolist
	  interleave-element)
  (open scheme)
  (open srfi-13)
  (open srfi-23)
  (open encodings)
  (files "httpd-utils.scm"))

(define-structure httpd
  (export httpd
	  stop-httpd

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

  (open httpd-utils)

  (files "httpd.scm"))

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

  (open httpd)
  (open httpd-utils)

  (files "httpd-servlet.scm"))
