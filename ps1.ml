(*
*Solution to exercise 1 of problem 4
*Returns a list containing every nth element of the list
*in the same order as they appear in this list
*Argument: lst (input list), n (the interval of the element)
*Precondition: N/A
*Postcondition: Returns a list the elements if the length of list is greater than n
*               Returns a empty list if n is greater than the list's length
*)
let every_nth lst n =
    let rec helper i = function
        | [] -> []
        | hd::tl ->
            if i=n then
                hd::helper 1 tl
            else
                helper (i+1) tl in
    helper 1 lst

(* 
*Solution to exercise 2 of problem 4
*Returns true when the list is unimodal, false otherwise.
*Argument: lst is an int list and ifPeak indicates wether 
*the current element is after the peak or not.
*Precondition: input should be a int list or empty list.
 *)
let is_unimodal lst =
    let rec aux_fun ifPeak = function
        |[] -> true
        |hd::[] -> true
        |hd1::hd2::tl ->
            (if ifPeak then (
                if hd1<hd2 then false else aux_fun true (hd2::tl)
            )
             else (
                if hd1>hd2 then aux_fun true (hd2::tl) else aux_fun false (hd2::tl)
            )) in
    aux_fun false lst

(* 
*Solution to exercise 3 of problem 4
*Returns a list of pairs of non-empty lists.
*Argument: lst is the input list
*Precondition: input should be a' list.
*Postcondition: if the input list is empty, it should return an empty list 
*negative reversed value.
 *)
let rec helper acc n i = function
   |[] -> ([], [])
   |hd::tl -> if i=n then (List.rev acc, hd::tl) else helper (hd::acc) n (i+1) tl

(* main function *)
let complete_list lst =
    let rec helper2 l acc i= 
    	if i = List.length lst then acc else helper2 l ((helper l i)::acc) (i+1)
    in helper2 lst [] 1       

(* 
*Solution to exercise 4 of problem 4
*Returns an integer whose digits are the reverse of the input integer.
*Argument: num is the input integer, acc is the accumulator.
*Precondition: input should be a int integer.
*Postcondition: if the input is a negative integer then it returns the 
*negative reversed value.
 *)
let rec helper num acc =
    if num<0 then (-(helper (-num) acc)) else
    (
        match num with
        | 0 -> acc
        | _ -> helper (num/10) (10*acc + (num mod 10))
    )

let rev_int num = helper num 0

(* 
*Solution to exercise 5 of problem 4
*Returns a list of lists, each of size k.
*Argument: lst is the input list, acc is the accumulator, subAcc is the accumulator
*of each subList, k is the size of subList
*Precondition: input should be 'a list.
*Postcondition: if the size k<=0, then None should be returned, if the list is empty,
*an empty list of lists should be returned.
 *)

let rec helper lst k i (acc : 'a list list) subAcc =
    match lst with
    | [] -> (
    			match subAcc with
    			| [] -> List.rev acc
    			| hd::tl -> List.rev ((List.rev subAcc)::acc)
    		)
    | h::t -> (
                if i=k then helper t k 1 ((List.rev (h::subAcc))::acc) [] else
                    helper t k (i+1) acc (h::subAcc)
                )

let unflatten k lst =
	if k<=0 then None else
	(
		if k>List.length lst then Some ([lst]) else
		(
			match lst with
			| [] -> Some ([[]])
    		| hd::t -> Some (helper lst k 1 [] [])
		)
	)

(* 
*Solution to exercise 6 of problem 4
*Returns the real value of a roman numeral.
*Argument: lst is the input list of roman numeral
*Precondition: input should be a roman numeral list.
 *)
type numeral = I | V | X | L | C | D | M
type roman = numeral list

let rec int_of_roman (r:roman):int =
    let int_of_numeral = function
        | I -> 1
        | V -> 5
        | X -> 10
        | L -> 50
        | C -> 100
        | D -> 500
        | M -> 1000 in
        let helper acc lst=
            match lst with
            | [] -> acc
            | h::[] -> acc + int_of_numeral h
            | h1::h2::t -> (
                if int_of_numeral h1>= int_of_numeral h2 then (acc + int_of_numeral h1 + int_of_roman (h2::t)) else
                    (acc - int_of_numeral h1 + int_of_roman (h2::t))
                )
        in helper 0 r;;
