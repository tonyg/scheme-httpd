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

(define flatten-iolist
  (let ()
    (define (flatten xs tail)
      (if (pair? xs)
	  (flatten (car xs) (flatten (cdr xs) tail))
	  (cons xs tail)))
    (lambda (iol)
      (string->utf-8 (string-concatenate (flatten iol '()))))))

(define (interleave-element element xs)
  (if (null? xs)
      '()
      (let loop ((xs xs))
	(if (null? (cdr xs))
	    xs
	    (cons (car xs) (cons element (loop (cdr xs))))))))

(define unquote-http-url
  (let ()
    (define (hex-char->digit c)
      (cond
       ((char-numeric? c) (- (char->integer c) (char->integer #\0)))
       ((char-lower-case? c) (+ 10 (- (char->integer c) (char->integer #\a))))
       ((char-upper-case? c) (+ 10 (- (char->integer c) (char->integer #\A))))
       (else #f)))
    (define (escaped-char->char ch1 ch2)
      (and-let* ((v1 (hex-char->digit ch1))
		 (v2 (hex-char->digit ch2)))
	(integer->char (+ (* v1 16) v2))))
    (lambda (str)
      (list->string
       (let loop ((chars (string->list str)))
	 (cond
	  ((null? chars) '())
	  ((and (eqv? (car chars) #\%)
		(pair? (cdr chars))
		(pair? (cddr chars)))
	   (cons (escaped-char->char (cadr chars) (caddr chars))
		 (loop (cdddr chars))))
	  (else
	   (cons (car chars) (loop (cdr chars))))))))))

(define (query-parameter-push alist key value)
  (call-with-values (lambda () (break (lambda (elt) (eq? (car elt) key)) alist))
    (lambda (head tail)
      (if (null? tail)
	  (if (eq? value #f) ;; ...&param&..., ie. no value
	      (cons (list key) alist)
	      (cons (list key value) alist))
	  (if (eq? value #f)
	      alist
	      (cons (cons key (append (cdar tail) (list value))) (append head (cdr tail))))))))

(define (parse-query query)
  (let ((kvs (string-split query '(#\&))))
    (fold (lambda (kv acc) (query-parameter-push acc (car kv) (cadr kv)))
	  '()
	  (map (lambda (kv) (split-header-by '(#\=) kv)) kvs))))

(define (split-header-by separators line)
  (apply (lambda (key . maybe-value)
	   (list (string->symbol (string-downcase (string-trim-both key)))
		 (if (null? maybe-value)
		     #f
		     (string-trim-both (car maybe-value)))))
	 (string-split line separators 1)))
