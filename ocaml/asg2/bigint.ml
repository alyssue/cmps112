(* ) 
Alyssa Melton
amelton@ucsc.edu
CMPS112 - Fall 2018
Wesley Mackey
Ags2 in OCAML
( *)

open Printf 

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])
    let llength   = List.length
    let lcompare  list1 list2 = 
        if (llength list1) < (llength list2)
        then (-1)
        else if (llength list1) > (llength list2)
        then 1
        else 0

(* ------------------------------------------------ *)

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let rec string_of_bigint' value position =
        if (position = 0) 
        then (string_of_int (car value):: (string_of_bigint' (cdr value) (position+1))) 
        else (
            match value, (position mod 69) with
            | [], _         -> []
            | car::cdr, 0   -> "\\ \n" ::string_of_int (car):: (string_of_bigint' (cdr) (position+1))
            | car::cdr, _   ->  string_of_int (car):: (string_of_bigint' (cdr) (position+1)))


    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (string_of_bigint' reversed 0))



(* -CMP-------------------------------------------- *)
(* ) compares bigint1 and bigint2. 
If int1 > int2, return 1
If int1 < int2, return -1
If int1 = int2 ,return 0 *)

    let rec cmp' rlist1 rlist2 =
        match (compare rlist1 rlist2) with
        | -1 -> -1                                    (* if rlist1 < rlist2 *)
        | 1  -> 1                                    (* if rlist1 > rlist2 *)
        | _  ->
            if (llength rlist1 = 1)                    (* if end of lists *)
            then 0
            else cmp' (cdr rlist1) (cdr rlist2)

    let cmp list1 list2 =
        match (lcompare list1 list2) with
        | -1 -> -1                                    (* if length.list1 < length.list2 *)
        | 1  -> 1                                    (* if length.list1 > length.list2 *)
        | _  ->                                            (* if length.list1 = length.list2*)        
            if (llength list1 = 0)                    (* if length.list1 & 2 = 0 *)
            then 0
            else                    
                (cmp' (reverse list1) (reverse list2))        (* compare values from highest value digit on *)

(* -GOODBYE-ZEROS------------------------------------ *)
(* ) Make sure that you always canonicalize your answers by deleting leading 0 digits. *)

    let rec goodbyezeros' numba =
        match numba with 
        | []       -> []
        | 0::cdr   -> goodbyezeros' cdr
        | car::cdr -> numba

    let goodbyezeros numba =
        let rnumba = reverse numba in 
            reverse (goodbyezeros' rnumba)

(* -THE PRIMES / HELPER FUNCS------------------------ *)
(* ) Recursive functions called on by implimented functions 
     that do the brute work for the function called *)

let rec add' list1 list2 carry = 
        match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  (sum mod radix) :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 borrow =
        match (list1, list2, borrow) with
        | [], list2, borrow   -> []
        | list1, [], borrow   -> sub' list1 [(borrow)] 0
        | [], [], borrow      -> []
        | car1::cdr1, car2::cdr2, borrow ->
            let difference = car1 - car2 - borrow in 
            if (difference < 0) then ((difference + radix) :: sub' cdr1 cdr2 1)
        else difference :: (sub' cdr1 cdr2 0)

    let rec linebyint value1 num carry = 
        match (value1, num, carry) with
        | value1, 1, carry    -> value1
        | value1, 0, carry    -> []
        | [], num, carry     -> [carry]
        | car1::cdr1, num, carry ->
          let prod = (car1 * num) + carry
          in (prod mod radix) :: linebyint cdr1 num (prod / radix)

    let rec mul' value1 value2 = 
        match (value1, value2) with
            | [], value2    -> []
            | [1], value2    -> value2
            | value1, []    -> []
            | value1, car2::cdr2 ->
                add' (linebyint value1 car2 0) (0::(mul' value1 cdr2)) 0


    let double number = add' number number 0

(* -EGYPTDIV-------------------------------------------- *)
(*  NOTES:
    comparable = value1 minus highest level doubles of value2 in each recursion
    answer = sum of eligible track2times2s

    bignum = bigger number we're dividing up by smallnum
    smallnum = changes with recursion into answer. is what we get when divide
    tracktimes2 = tracks how many times we go up by 2, is the added portions
    comparable = bignum - all the levels 
    that were able to fit insdie new comparable after first
*)

    let rec egyptdiv' bignum smallnum tracktimes2 = 
        (* at top of chart, end and return original value to start comparing from *)
        if (cmp bignum smallnum) = -1 then (bignum, [0]) 
        (* once we get to the top of chart, recur from top down *)
        else let comparable, answer = (egyptdiv' bignum (double smallnum) (double tracktimes2)) in
            (* if smallnum is greater than comparable we canNOT subtract *)
            if (cmp comparable smallnum) = -1 then comparable, answer
            else
            (* when highest level below comparable (value2-old highest level) 
            new comparable = (sub' bignum value2times2) 
            the answer / sum of tracktimes2 = (add' oldtrack2times2 tracktimes2 0) *)
            (goodbyezeros (sub' comparable smallnum 0)), (add' answer tracktimes2 0)
        

(* --SUBTRACT--------------------------------------------- *)
(* ) When you subtract, make sure that the first argument is always the larger one. *)

    let sub (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
    if (sign1 = sign2)
        then (match sign1 with                                    (* if signs are the same *)
        | Pos -> (match (cmp value1 value2) with
                | -1 -> Bigint (Neg, goodbyezeros (sub' value2 value1 0))        (* if value1 < value2*)
                | 1  -> Bigint (Pos, goodbyezeros (sub' value1 value2 0))        (* if value1 > value2*)
                | _  -> zero)
            | Neg -> (match (cmp value1 value2) with
                | -1 -> Bigint (Pos, goodbyezeros (sub' value2 value1 0))    (* if value1 < value2*)
                | 1  -> Bigint (Neg, goodbyezeros (sub' value1 value2 0))    (* if value1 > value2*)
                | _  -> zero))
    else (match sign1 with                            (* if the signs are different *)
        | Pos -> Bigint (Pos, add' value1 value2 0)             (* Pos - Neg = Pos *)
        | Neg -> Bigint (Neg, add' value1 value2 0))         (* Neg - Pos = Neg *)


(* -ADD---------------------------------------------------- *)

    let add (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        if sign1 = sign2
        then Bigint (sign1, add' value1 value2 0)
        else (match cmp value1 value2 with                          (* if the signs are different *)
            | -1 -> Bigint (sign2, goodbyezeros (sub' value2 value1 0))          (* Pos - Neg = Pos *)
            | 1  -> Bigint (sign1, goodbyezeros (sub' value1 value2 0))         (* Neg - Pos = Neg *)
            | _  -> zero)

(* --MULTIPLY-----------------------------------------------*)

    let mul (Bigint (sign1, value1)) (Bigint (sign2, value2)) = 
    if sign1 = sign2
    then (match (cmp value1 value2) with
            | -1 -> Bigint (sign1, mul' value2 value1)        (* if value1 < value2*)
            | _  -> Bigint (sign1, mul' value1 value2))        (* if value1 >= value2*)
        else (match (cmp value1 value2) with
            | -1 -> Bigint (Neg, mul' value2 value1)            (* if value1 < value2*)
            | _  -> Bigint (Neg, mul' value1 value2))            (* if value1 >= value2*)

(* --DIVIDE-------------------------------------------------*)
    
    let div' value1 value2 = 
        match (egyptdiv' value1 value2 [1]) with
        | _, answer -> answer


    let div (Bigint (sign1, value1)) (Bigint (sign2, value2)) = 
        let answer = div' value1 value2 in
        if sign1 = sign2
    then Bigint (sign1, answer)    
        else Bigint (Neg, answer)

(* --REMAINDER---------------------------------------------- *)


    let rem' value1 value2 = 
        match (egyptdiv' value1 value2 [1]) with
        | answer, _ -> answer


    let rem (Bigint (sign1, value1)) (Bigint (sign2, value2)) = 
        let answer = rem' value1 value2 in
            Bigint (Pos, answer)

(* --EXPONENTIATE------------------------------------------- *)
(* ) x to the power of n *) 

    let even value1 = 
    let remainder = rem' value1 [2]
    in remainder = []

    let square x =
    mul' x x

    let half x =
    div' x [2] 
 

    let rec pow' x n = 
        match (x, n) with 
        | [1], n -> [1]
        | [0], n -> [0]
        | x, [0] -> [1]
        | x, [1] -> x
        | x, n   -> 
            if (even n) 
            then pow' (square x) (half n)
            else mul' x (pow' (square x) (half (sub' n [1] 0)))


    let pow (Bigint (sign1, x)) (Bigint (sign2, n)) = 
    if sign2 = Pos 
    then (
        (* if x is Pos, return Pos *)
        if sign1 = Pos then Bigint (Pos, goodbyezeros (pow' x n)) 
        (* if x is Neg, but powered to a pos num, return Pos *)
        else if (even x) then Bigint (Pos, goodbyezeros (pow' x n)) 
        (* if x is Neg, but powered to a neg num, return Neg *)
        else Bigint (Neg, goodbyezeros (pow' x n)) 
        )
    else zero


end




