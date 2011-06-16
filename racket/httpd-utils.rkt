#lang racket

(require srfi/2)
(require srfi/13)

(provide string-split
         flatten-iolist
         interleave-element
	 unquote-http-url)

(define string->utf-8 string->bytes/utf-8)

(include "../portable/httpd-utils.scm")
