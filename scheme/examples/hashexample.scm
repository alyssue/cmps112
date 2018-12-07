#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: hashexample.scm,v 1.2 2014-10-31 17:35:08-07 - - $
;;
;; Another hash table example, showing insertion of variables,
;; vectors, and functions, and checking for lookup.
;; Note the script above uses -qr instead of -qC.
;;

;;
;; A little utility function to make printing easier.
;; Mz Scheme does have a printf function, but we'll stick to
;; standard Scheme here.
;;
(define (show label it)
    (display label)
    (display " = ")
    (display it)
    (newline)
)

;;
;; Create a hash table and put in some functions and variables.
;;
(define ht (make-hash))
(for-each
    (lambda (item) (hash-set! ht (car item) (cadr item))) ;; lambda introduces a function. the item is a list of length 2. car key cadr value
    `((var 34)
      (+ ,(lambda (x y) (+ x y))) ;; takes two variables and pluses them. overrides the normal plus. can replace with ",+"
      (- ,(lambda (x y) (- x y)))
      (* ,*)  ;; comma unquotes things. means rvaluate the thing folowing the comma and use that as the value. * is the original procedure. 
      (vec ,(make-vector 10 0.0))) ;; create a vector, 10 things in it, all initialized to zero. 
)

;;
;; Print the hash table.
;;
(hash-for-each ht (lambda (key value) (show key value))) ;; must be a func that takes two args, key and value. calls show. which prints key and value. 
(newline) ;;hash-for-each is useful for debugging but pointless for programming. 

;;
;; show the value of a simple variable.
;; the last argument #f causes hash-ref to return it
;; rather than crashing the program on failure to find.
;;
(show "var" (hash-ref ht 'var #f)) ;; hash ref looks up something in hash table. find symbol associate with 'var and if not there then #f. 

;;
;; Set a vector element, print it, and the whole vector.
;;
(vector-set! (hash-ref ht 'vec #f) 5 3.1415926535)  ;; modifies the thing in vector. find symbol vec and return f if not there. then element 5 get set to pi aproximation.
(show "vec[5]" (vector-ref (hash-ref ht 'vec) 5)) ;; # nice
(show "vec" (hash-ref ht 'vec #f))

;;
;; A couple more examples.
;;
(show "(+ 3 4)" (apply (hash-ref ht '+ #f) '(3 4))) ;; if u have func and list. you can call function on every element in list by using apply. 
(show "not found" (hash-ref ht 'foo #f)) ;; say you have (f a b)

;;
;; The function evalexpr outlines how to evaluate a list
;; recursively.
;;
(define (evalexpr expr)                           ;; want to evaluate expression. is a number or symbol or list. 
   (cond ((number? expr) expr)      ;; is the expression a number? then return number (expression)
         ((symbol? expr) (hash-ref ht expr #f)) ;; symbol? then look up the symbol in the hash table and return whatever we find. don't find it, then #f
         ((pair? expr)   (apply (hash-ref ht (car expr)) ;; is pair? apply that func to list. 
                                (map evalexpr (cdr expr))))
         (else #f))
)

;;
;; Now print out the value of several expressions.
;;
(for-each
    (lambda (expr) (show expr (evalexpr expr)))
    '( (* var 7)
       (- 3 4)
       (+ (* var 7) (- 3 4))
))

;;
;; Just to verify that we got all the way.
;;
(display "DONE.") (newline)

;;-----------------------------------------------------------------------------

ANY division by zero results in +inf or -inf or NAN. no worry for complexs. 

Don't use list? because it's an order n operation and will take long time and suck . ask if you have a pair, not list. 

say you have 
l = '(f a b)
(apply f (map eval (cdr l)))

