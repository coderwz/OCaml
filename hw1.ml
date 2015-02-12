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

(* helper function: return a tuple in which the first list has n elements. *)
 let rec helper acc n i = function
    |[] -> ([], [])
    |hd::tl -> if i=n then (List.rev acc, hd::tl) else helper (hd::acc) n (i+1) tl

(* main function *)
let complete_list lst =
    let rec helper2 l acc = function 
        | List.length lst -> acc
        | _ -> helper2 l ((helper l i)::acc) (i+1)
    in helper2 lst [] 1


Ex4:


let rec helper num acc = 
    if num<0 then (-(helper (-num) acc)) else
    (
        match num with
        | 0 -> acc
        | _ -> helper (num/10) (10*acc + (num mod 10))
    )

let rev_int num = helper num 0

Ex5:

let rec helper lst k i (acc : 'a list list) subAcc = 
    match lst with
    | [] -> List.rev (subAcc::acc)
    | h::t -> (
                if i=k then helper t k 1 ((List.rev (h::subAcc))::acc) [] else
                    helper t k (i+1) acc (h::subAcc)
                )

let unflatten lst k = 
    match lst with
    | [] -> None
    | hd::t -> Some (helper lst k 1 [] [])


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
        let helper acc lst= 
            match lst with
            | [] -> acc
            | h::[] -> acc + int_of_numeral h
            | h1::h2::t -> (
                if int_of_numeral h1>= int_of_numeral h2 then (acc + int_of_numeral h1 + int_of_roman (h2::t)) else
                    (acc - int_of_numeral h1 + int_of_roman (h2::t))
                )
        in helper 0 r;;
            

















