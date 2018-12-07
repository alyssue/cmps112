#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.4 2018-04-11 16:31:36-07 - - $
;; --------------------------------------------------------------------------------------
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; 
;; Alyssa Melton
;; amelton@ucsc.edu
;; CMPS 112 -- Comparative Programming Languages
;; Assignment 1
;; Scheme
;;
;; Name
;; sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;; sbi.scm filename.sbir
;;
;; DESCRIPTION
;; The file mentioned in argv[1] is read and assumed to be an SBIR
;; program, which is the executed.  It is interpreted.
;;
;;
;;
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; --------------------------------------------------------------------------------------


;; --------------------------------------------------------------------------------------
;; GENERICS
;; --------------------------------------------------------------------------------------

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; --------------------------------------------------------------------------------------
;; Identifier-Table
;; --------------------------------------------------------------------------------------

(define *identifier-table* (make-hash))
(define (get-identifier! key)
        (hash-ref *identifier-table* key))
(define (add-identifier! key value)
        (hash-set! *identifier-table* key value))

(for-each
        (lambda (pair)
                (add-identifier! (car pair) (cadr pair)))
        `(
                (input-count      0)
                (dim              interpret-dim)
                (let              interpret-let)
                (goto             interpret-goto)
                (if               interpret-if)
                (print            interpret-print)
                (input            interpret-input)
                (pi               3.141592653589793238462643383279502884197169399)
                (log10_2          0.301029995663981195213738894724493026768189881)
                (sqrt_2           1.414213562373095048801688724209698078569671875)
                (e                2.718281828459045235360287471352662497757247093)
                (div              ,(lambda (x y) (floor (/ x y))))
                (log10            ,(lambda (x) (/ (log x) (log 10.0))))
                (%                ,(lambda (x y) (truncate (/ x y))))
                (quot             ,(lambda (x y) (truncate (/ x y))))
                (mod              ,(lambda (x y) (- x (* (div x y) y))))
                (rem              ,(lambda (x y) (- x (* (quot x y) y))))
                (+                ,+)
                (-                ,-)
                (*                ,*)
                (/                ,/)     
                (<=               ,<=) 
                (>=               , >=) 
                (=                ,=) 
                (>                ,>) 
                (<                ,<)
                (<>               ,(lambda (x y) (not (= x y))))        
                (^                ,expt)
                (ceil             ,ceiling)
                (exp              ,exp)
                (floor            ,floor)
                (log              ,log)
                (sqrt             ,sqrt)
                (abs              ,abs)
                (sin              ,sin)
                (cos              ,cos) 
                (tan              ,tan) 
                (atan             ,(lambda (x) (atan x)))  
                (asin             ,asin) 
                (acos             ,acos) 
                (round            ,round) 
         )
)

;; --------------------------------------------------------------------------------------
;; Label-Table
;; --------------------------------------------------------------------------------------

(define *label-table* (make-hash))
(define (get-label key)
        (hash-ref *label-table* key))
(define (add-label! key value)
        (hash-set! *label-table* key value))

;; --------------------------------------------------------------------------------------
;; readlist-from-inputfile
;; --------------------------------------------------------------------------------------

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename))) ;;assign inputfile the value of the input file. open it and make the input port
         (if (not (input-port? inputfile)) ;;if it doesn't create an input port,
             (die `(,*run-file* ": " ,filename ": open failed")) ;; ERROR aka DIE and exit. 
             (let ((program (read inputfile))) ;; else, var program = inputfile data
                  (close-input-port inputfile) ;; close the port, now program holds the data from inputfile.
                         program))))

;; --------------------------------------------------------------------------------------
;; Get-Labels
;; --------------------------------------------------------------------------------------

(define (get-labels linelist)
        (define line (car linelist))
        (when (> (length line) 1)                            ;; if label or statement exists (more elements after line number)
              (when (symbol? (cadr line))                    ;; if symbol
                    (add-label! (cadr line) linelist)))      ;; label: add it
        (when (> (length linelist) 1)                        ;; more lines to read?
              (get-labels (cdr linelist))))                  ;; read them!


;; --------------------------------------------------------------------------------------
;; I N T E R P R E T - S T A T E M E N T S 
;; --------------------------------------------------------------------------------------

;; -evaluate expression------------------------------------------------------------------

(define (evaluate-expression expression)
        (cond ((string? expression) expression ) ;; is a string so simply return it to print
              ((number? expression) (+ 0.0 expression))
              ((list? expression)
                      (cond ((hash-has-key? *identifier-table* (car expression))
                              (cond ((vector? (get-identifier! (car expression))) 
                                              (define vector (get-identifier! (car expression)))
                                              (define index (inexact->exact (get-identifier! (cadr expression))))
                                              (vector-ref vector index))
                              (else (apply (get-identifier! (car expression)) (map evaluate-expression (cdr expression))))) 
                            )
                      )
              )
              ((hash-has-key? *identifier-table* expression) (get-identifier! expression))
        (else expression)
        )
)


;; -'dim'--------------------------------------------------------------------------------

(define (interpret-dim statement linelist)
        (define variable-expression (cadr statement))
        (define array-name (car variable-expression))
        (define dimension (evaluate-expression (cadr variable-expression)))
        (add-identifier! array-name (make-vector (inexact->exact dimension)))

        (when (> (length linelist) 1)                 
              (interpret-program (cdr linelist))))    ;; recur / continue



;; -'let'--------------------------------------------------------------------------------

(define (interpret-let statement linelist)
        (define key (cadr statement))
        (define value (evaluate-expression (caddr statement)))

        (cond ((symbol? key) (add-identifier! key value)) ;; if not a pair, variable, add like normal.
        (else 
              (define vector (get-identifier! (car key)))
              (define index (get-identifier! (cadr key)))
              (vector-set! vector (inexact->exact index) value)))

        (when (> (length linelist) 1)                 
              (interpret-program (cdr linelist))))  ;; recur / continue


;; -'goto'-------------------------------------------------------------------------------

(define (interpret-goto args linelist)
        (define label (cadr args))
        (when not (null? (get-label label))
              (interpret-program (get-label label))))


;; -'if'---------------------------------------------------------------------------------

(define (interpret-if statement linelist)
        ;; statement will look something like (if (relop expression expression) label)
        (define relop-expression (cadr statement))
        (define label (caddr statement))
        (cond ((evaluate-expression relop-expression) (interpret-program (get-label label)))
        (else 
              (when (> (length linelist) 1)                 
                    (interpret-program (cdr linelist))))    ;; recur / continue
))
   

;; -'print'------------------------------------------------------------------------------

(define (evaluate-printables printables) 
        (define expression (car printables))
        (display (evaluate-expression expression))
        (when (> (length printables) 1)    
              (display " ")             
              (evaluate-printables (cdr printables)))
)


(define (interpret-print statement linelist) ;; 
        (when (not (= (length statement) 1))  ;; when printables exist, not only keyword "print"
                  (define printables (cdr statement))
                  (evaluate-printables printables)
                  (display "\n")
        )
        (when (> (length linelist) 1)                 
              (interpret-program (cdr linelist))))    ;; recur / continue



;; -'input'------------------------------------------------------------------------------

(define (interpret-input args linelist)
        (define num (get-identifier! 'input-count))
        (add-identifier! 'input-count (+ num 1))

        (when (> (length linelist) 1)                 
              (interpret-program (cdr linelist))))    ;; recur / continue


;; --------------------------------------------------------------------------------------
;; Interpret-Program
;; --------------------------------------------------------------------------------------

(define (interpret-program linelist)
        (define line (car linelist))
        ;; if only linenumber
        (when (= (length line) 1)    
              (when (> (length linelist) 1)                 
                    (interpret-program (cdr linelist))))    ;; recur / continue

        ;; if two elements in line
        (when (= (length line) 2)
              (if (list? (cadr line))                   
                  (which-statement (cadr line) linelist)          ;; if statement but no label
                  (when (> (length linelist) 1)                 
                        (interpret-program (cdr linelist)))))     ;; else recur / continue
                    
        ;; if three elements in line, then statement
        (when (= (length line) 3)
             (when (pair? (cddr line))   
                    (define statement (caddr line))
                    (which-statement statement linelist))))

;; helper statement finder
(define (which-statement statement linelist)
        (define keyword (car statement))
              (cond 
                      ((eqv? keyword 'dim)
                            (interpret-dim statement linelist))
                      ((eqv? keyword 'let)
                            (interpret-let statement linelist))
                      ((eqv? keyword 'goto)
                            (interpret-goto statement linelist))
                      ((eqv? keyword 'if)
                            (interpret-if statement linelist))
                      ((eqv? keyword 'print)
                            (interpret-print statement linelist))
                      ((eqv? keyword 'input)
                            (interpret-input statement linelist)))) 

;; --------------------------------------------------------------------------------------
;; MAIN
;; --------------------------------------------------------------------------------------


(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist)))) ;; if arglist is null, OR there's more than one argument
        (usage-exit) ;; exit. if not, 
        (let* ((sbprogfile (car arglist)) ;; let the sbprogfile
               (program (readlist-from-inputfile sbprogfile))) ;; program is list w/ sbprogfile lines
        (get-labels program)
        (add-identifier! 'input-count 0)
        (interpret-program program)
)))

;; -run and exit-------------------------------------------------------------------------------------

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))


