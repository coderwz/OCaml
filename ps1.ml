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