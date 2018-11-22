#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.4 2018-04-11 16:31:36-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

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
;; readlist-from-inputfile
;; filename = sbir filename
;; description: create a list called "program" that holds each line of the sbir file.

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename))) ;;assign inputfile the value of the input file. open it and make the input port
         (if (not (input-port? inputfile)) ;;if it doesn't create an input port,
             (die `(,*run-file* ": " ,filename ": open failed")) ;; ERROR aka DIE and exit. 
             (let ((program (read inputfile))) ;; else, var program = inputfile data
                  (close-input-port inputfile) ;; close the port, now program holds the data from inputfile.
                         program))))


;; --------------------------------------------------------------------------------------
;; write-program-by-line
;; filename = sbir filename
;; program = list of sbir file's lines
;; description: takes sbir file and prints it!

(define (write-program-by-line filename program) ;; new function "write-program-by-line" that takes in the filename and the filedata(program) as args
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program) ;; for every line in program, print that line
    (printf ")~n"))


;; ---------------------------------------------------------------------------------------
;; EXCECUTE THE DEFINED FUNCTION


(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist)))) ;; if arglist is null, OR there's more than one argument
        (usage-exit) ;; exit. if not, 
        (let* ((sbprogfile (car arglist)) ;; let the sbprogfile
               (program (readlist-from-inputfile sbprogfile))) ;; program is list w/ sbprogfile lines
              (write-program-by-line sbprogfile program)))) ;; now write file by line! w/ filename=sbprogfile and program=program.


























;; --------------------------------------------------------------------------------------
;; Program Exit

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))
