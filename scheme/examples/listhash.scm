#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: listhash.scm,v 1.3 2015-10-09 14:16:37-07 - - $

;;
;; NAME
;;    listhash - put some entries in a list into a hash table
;;

(define *hash* (make-hash))

(define *list*
    '(  (label (foo bar))
        (      (line 2))
        (sec   (sec line))      ;; car is entire line, but caar (car of the car) is "sec"
        (      (line 4))
        (last  (label))
        (      (end))))

(define (show label item)
        (newline)
        (display label) (display ":") (newline)
        (display item) (newline))

(define (put-in-hash list)
        (when (not (null? list))                ;; when the list is not null
              (let ((first (caar list)))        ;; do the let thing and after that do tail recursive call on the cdr of the list,, caar is car of the car.
                   (when (symbol? first)
                         (hash-set! *hash* first list)))
              (put-in-hash (cdr list))))        ;; cdr of the list.

(show "whole list" *list*)

(put-in-hash *list*)

(hash-for-each *hash*
    (lambda (key value) (show key value)))


;; --------------------------------------------------------------------------------------
;; NOTES 
;; --------------------------------------------------------------------------------------

if you want to loop similarly to for or while, you can use map or for-each etc
 
you can use: 
when // gives condition, does the thing if condition is true. 
if // scheme if requires an else
cond // run down a checklist? 


example: 
(define (haszero list)
        (if (null? list))#f
            (if (= (car list) 0)
                (haszero (cdr list))))

better way:

(define (haszero list)
        (cond ((null? list) #f)
              ((= (car list) 0) #t
                  (else (haszero (dcr list))))))








