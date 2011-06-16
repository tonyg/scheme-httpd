#lang racket

(require srfi/1)
(require srfi/13)

(require "httpd-utils.rkt")
(require "httpd.rkt")

(provide dispatch-rules
	 dispatch-case
	 dispatch-url
	 binding-let)

(include "../portable/httpd-servlet.scm")
