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
