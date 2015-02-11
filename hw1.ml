1.c 
    let f x a = x::a in f 42 [3110]
    -->
    lef f = function (x:int) --> function (a:int list) --> x::a
    -->
    let f [3110]= (function (a:int list) --> 42::a) (because {42/x})
    -->
    let f = [42; 3110] (because {[3110]/a})
    -->
    [42; 3110]

Problem 2:
    a. "string1"^"string2"
    b. 1::[2]
    c. let x = "head" in x::[e1; e2]
    d. let x = ["str1", "str2", "str2"] in match x with
        |[] --> "list1"
        |"str1"::tail --> "list2"
        |hd:tl --> "list3"

    e. let x =1 in let y = 1 in x+y;;
    f. let x=0 in let y='a' in 1+2
    g. let x=(0, 'a') in 1
    h. {first_name = "Wen"; last_name = "Zhu"; gpa = 4.0}
    i. let x = {first_name = "Wen"; last_name = "Zhu"; gpa = 4.0} in (x.first_name, x.last_name)

    (* Problem exists *)
    j. let x = "Wen" in let y = "Zhu" in let z = 4.0 in {first_name = x;
                 last_name = y; gpa = z}

Problem 3:

let reverseList1 (x: 'a list):'a list = 
    let rec reverseList2 (y: 'a list)(z: 'a list):'a list = 
        match y with
        | [] -> []
        | hd::tl -> reverseList2 tl [hd]@z
in reverseList2 x []



Problem 4:

Ex1:

let every_nth lst n =
    let rec aux_fun i = function
        | [] -> []
        | hd::tl -> if i=n then hd::aux_fun 1 tl else aux_fun (i+1) tl in
    aux_fun 1 lst;;

testcase:
every_nth [] 1
every_nth [1;2;3;4;5;6] 3
every_nth ['a';'b';'c';'d'] 4

Ex2:

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
    aux_fun false lst;;


Ex3:

(* helper function: return a tuple in which the first list has n entries. *)
 let helper lst n = 
    let rec findTuple aux i = function
        |[] -> ()
        |hd::tl -> if i=n then (List.rev aux, tl) else findTuple hd::aux (i+1) tl
 in findTuple [] 0 lst


(* main function *)
let rec complete_list helper acc lst i= 
    match i with
    | List.length lst -> helper lst i::acc
    | _ -> acc::complete_list helper (helper lst i)::acc lst (i+1)


Ex4:

let rec rev_int num acc digit = 
    if rev_int<0 then -(rev_int -num acc digit) else
    (
        match num with
        | 0 -> acc
        | _ -> acc + (num mod 10)*(10**digit) + 
            rev_int (num/10) (acc + (num mod 10)*(10**digit) (digit+1)
    )

Ex5:

let rec unflatten lst k i acc subAcc= 
    if k<=0 then None else
    (
        match lst with
        | [] -> Some (List.rev subAcc::acc)
        | h::t -> (
                    if i=k then unflatten t k 1 (List.rev h::subAcc)::acc [] else
                        unflatten t k (i+1) acc h::subAcc
                )
    )

Ex6:

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
        let helper  = 

















