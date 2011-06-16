#lang racket
(require "httpd-utils.rkt")
(require rackunit)

(display "Testing httpd-utils...")
(newline)

(check-equal? (unquote-http-url "/foo%25/") "/foo%/")
(check-equal? (unquote-http-url "/foo%22/") "/foo\"/")
(check-equal? (unquote-http-url "/foo%2b%2a/") "/foo+*/")
