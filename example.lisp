;; Define basic functions
(let [println (lambda [expr] (do ((primitive println) expr)))
     + (lambda [a b] (do ((primitive +) a b)))
     - (lambda [a b] (do ((primitive -) a b)))
     * (lambda [a b] (do ((primitive *) a b)))
     / (lambda [a b] (do ((primitive /) a b)))
     = (lambda [a b] (do ((primitive =) a b)))
     > (lambda [a b] (do ((primitive >) a b)))
     < (lambda [a b] (do ((primitive <) a b)))
     cons (lambda [a b] (do ((primitive cons) a b)))
     car (lambda [cell] (do ((primitive first) cell)))
     cdr (lambda [cell] (do ((primitive rest) cell)))]
     (do 
      (println "Hello from metacyclic interpretor!")

      ;; Define some variables and a cons-list
      (let [a 1
	   b 2
	   cell (cons 7 (cons 5 nil))]
	   (do 
	    (println "1 + 2 = ")
	    (println (+ a b))

	     (println "Getting data from list...")
	     (println "5 + 7 =")
	     (println (+ (car cell) (car (cdr cell))))

	     ;; Define a function "abs"
	     (let [abs (lambda [num]
			 (do (if (< num 0)
				 (- 0 num)
				 num)))
		  ]	       
		  (do
		   (println "Abs of negative number (-10):")
		   (println (abs (- 0 10)))
		    (println "Abs of positive number (17):")
		    (println (abs 17))))))))
