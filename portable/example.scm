(define current-request (make-parameter #f))

(define (respond title body-parts)
  (make-http-response 200 "OK"
		      '(("Awesome" "oh yes"))
		      (parameterize ((xml-empty-tags-mode #f))
			(xxexpr->string
			 `((html (head (title ,title))
				 (body (h1 "Test")
				       ,@body-parts
				       (p "Return to the "(a ((href ,(page-url main-page)))
							     "main page")
					  "."))))))))

(define (main-page req)
  (respond "Test"
	   `((p "See "(a ((href ,(page-url other-page))) "here")".")
	     (p "Try the "(a ((href ,(page-url counter 0))) "counter")".")
	     (p "Alternatively, "(a ((href ,(page-url stop-page))) "stop")" the server."))))

(define (counter req n)
  (respond "Counter"
	   `((p "Current value: ",n)
	     (ul
	      (li (a ((href ,(page-url counter (+ n 1)))) "more"))
	      (li (a ((href ,(page-url counter (- n 1)))) "less")))
	     (p "Click "(a ((href ,(page-url sum-page (list 1 2 3 4 5 6 7 8 9 10))))
			   "here")" for the sum of the numbers 1 to 10."))))

(define (other-page req)
  (respond "Other"
	   `((form ((action ,(page-url post-target-page))
		    (method "POST"))
		   (input ((name "foo") (value "bar"))) (br)
		   (input ((name "zot") (value "quux"))) (br)
		   (input ((type "submit") (value "Do it")))))))

(define (post-target-page req)
  (respond "Target"
	   `((p "You posted:")
	     (ul
	      ,@(map (lambda (piece)
		       `(li ,(symbol->string (car piece))" = ",@(cdr piece)))
		     (parse-query (http-request-body-string req)))))))

(define (stop-page req)
  (stop-http-daemon daemon)
  (respond "Stopped"
	   `((p "It's stopped now."))))

(define (sum-page req numbers)
  (make-redirect-response
   (page-url counter (let sum ((numbers numbers))
		       (if (null? numbers)
			   0
			   (+ (car numbers) (sum (cdr numbers))))))))

(define dispatch-request '*)
(define page-url '*)
(call-with-values
    (lambda () (dispatch-rules
		(() main-page)
		(("other") other-page)
		(("post-target") post-target-page)
		(("stop") stop-page)
		(("counter" (integer-arg)) counter)
		(("sum" (number-arg) ... "those" "numbers") sum-page)
		(else main-page)))
  (lambda (dr du)
    (set! dispatch-request dr)
    (set! page-url du)))

(define daemon (make-http-daemon 8000 dispatch-request))
(run-http-daemon daemon)
