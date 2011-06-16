#lang racket
(require "httpd-utils.rkt")
(require rackunit)

(display "Testing httpd-utils...")
(newline)

(check-equal? (unquote-http-url "/foo%25/") "/foo%/")
(check-equal? (unquote-http-url "/foo%22/") "/foo\"/")
(check-equal? (unquote-http-url "/foo%2b%2a/") "/foo+*/")

(check-equal? (parse-query "") '())
(check-equal? (parse-query "foo") '((foo)))
(check-equal? (parse-query "foo&bar") '((bar) (foo)))
(check-equal? (parse-query "foo&foo&bar") '((bar) (foo)))
(check-equal? (parse-query "foo&bar&foo") '((bar) (foo)))
(check-equal? (parse-query "foo=123&bar&foo") '((bar) (foo "123")))
(check-equal? (parse-query "foo=123&bar&foo=123") '((foo "123" "123") (bar)))
(check-equal? (parse-query "foo=123&bar&foo=234") '((foo "123" "234") (bar)))
(check-equal? (parse-query "foo=123&bar=234&foo") '((bar "234") (foo "123")))
(check-equal? (parse-query "foo&bar=234&foo=123") '((foo "123") (bar "234")))
(check-equal? (parse-query "foo=123&bar=234&foo=345") '((foo "123" "345") (bar "234")))
