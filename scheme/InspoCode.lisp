


;; from hashexample.scm
(define (evalexpr expr)                           ;; want to evaluate expression. is a number or symbol or list. 
   (cond ((number? expr) expr)      ;; is the expression a number? then return number (expression)
         ((symbol? expr) (hash-ref ht expr #f)) ;; symbol? then look up the symbol in the hash table and return whatever we find. don't find it, then #f
         ((pair? expr)   (apply (hash-ref ht (car expr)) ;; is pair? apply that func to list. 
                                (map evalexpr (cdr expr))))
         (else #f))
)





;; from listhash.scm :
(define (put-in-hash list)
        (when (not (null? list))                ;; when the list is not null
              (let ((first (caar list)))        ;; do the let thing and after that do tail recursive call on the cdr of the list,, caar is car of the car.
                   (when (symbol? first)
                         (hash-set! *hash* first list)))
              (put-in-hash (cdr list))))        ;; cdr of the list.




;; from source
(define (read-program prgm)                                           ;; define a function that will get symbols from program
   (when (> (length (car prgm)) 1);not (null? (cdr(car prgm)))        ;; when length of program greater than 1
   (when not (null? (cdr prgm))                                       ;; when program is not null (valid)
         (when (symbol? (cadr (car prgm)))                                    ;; when get label is symbol
         ;we have a label, put it into the the label table                    ;; put label 
            (put-label! (cadr(car prgm)) prgm)                                ;; 
            (read-opt (cddr(car prgm)))
         )
         (when not (symbol? (cadr (car prgm)))
            ;no label, just pass in the function we're gonna do
            (read-opt (cdr(car prgm)))
         )
   ))
   (when not (null? (cdr prgm))
   (when (> (length (cdr prgm)) 0)
         (read-program (cdr prgm))
   ))
)


;; from source
(define (process-opt line)
 (when (eqv? line `()) (die (list "")))
   (define operator (car line))
   (cond
           [(eqv? operator 'dim)
              (handle-dim line)]
           [(eqv? operator 'let)
              (handle-let line)]
           [(eqv? operator 'goto)
              (handle-goto (cadr line))] 
           [(eqv? operator 'if)
              (handle-if line)]
           [(eqv? operator 'print)
              (handle-print line)]
           [(eqv? operator 'input)
              (handle-input line)]
         )
  ;); end when
)

;; from source
(define (read-opt line)                                                   ;; 
   (when (> (length line) 0)                                              ;; line length is not empty                             
         (cond
              [(eqv? (car line) 'goto)                                    ;; line = goto?
                 (put-function! (car line) (cdr line))]                   ;; create function goto!
              [(eqv? (car line) 'print)                                   ;; line = print?
                 (put-function! (car line) (cdr line))]                   ;; create function print!
              [(eqv? (car line) 'input)                                   ;; line = input?
                 (put-variable! 'inputcount                               ;; create variable inputcount
                  (+ (get-variable inputcount) (length (cdr line))))
                 (put-function! (car line) (cdr line))]
         )
   )
)

;; from source
(define (put-array! name index value)
   (vector-set! (get-variable name) index value) 
)


(define (read-program prgm)
   (when (> (length (car prgm)) 1)              ;; when length (car program) > 1
   (when not (null? (cdr prgm))                 ;; and when program has a cdr
         (when (symbol? (cadr (car prgm)))                  ;; if the second element of the line is a symbal       
              (put-label! (cadr(car prgm)) prgm)                    ;; put label 
              (read-opt (cddr(car prgm)))                           ;; read-opt statements t of the line
         )
         (when not (symbol? (cadr (car prgm)))              ;; if not a symbol, just pass in the function we're gonna do
            (read-opt (cdr(car prgm)))                              ;; read-opt the statements in first line
         )
   ))     
   (when not (null? (cdr prgm))                 ;; when length (car program) is not null
   (when (> (length (cdr prgm)) 0)              ;; when there are more lines of program left
         (read-program (cdr prgm))              ;; tail recursion
   ))
)



