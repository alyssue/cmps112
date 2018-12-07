;;-----------------------------------------------------------------------------
;; Scheme
;;-----------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
;; Higher Order Functions
;;-----------------------------------------------------------------------------

Tail Calls and tail recursions. 
functional languages don't have loops. 
instead of loops we use recursion but normal call nests functions on a stack
want length of list? 
// in c you'd use a loop
in scheme we mandate tail calls. 

optimizer translates tail calls into loops. 

closest to assembly is C. 




;;-----------------------------------------------------------------------------

NonTailCall

(def (fac n)
	(if (< n 1) 1
		(* n (fac (-n 1)))
	)
)

evaluate (fac 4) = ....
.. 
.. 
. 
.



deep stack in the end









;;-----------------------------------------------------------------------------

Tail Call

(def (fac n)
	(def (facc n a)				;; you can define functions within other functions. facc is the accumulator. 
		(if (< n 1) a
			(facc (-n 1)
				(* n a)))
		(facc n 1)
	)
)

(fac 4)
= (facc 4 1)
= (facc 3 4)
= (facc 2 12)
= (facc 1 24)
= (facc 0 24)
= 24

;;-----------------------------------------------------------------------------


what is minumum element of list? 

fold is some thing that i missed









