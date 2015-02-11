let rec sum (lst:int list) = 
    match lst with
    | [] -> 0
    | h::t -> h + sum t;;

let rec concat (l string list) = 
    match l with
    | [] -> ""
    | h::t -> h^concat t;;

let rec sum' l acc = 
    match l with
    | [] -> acc
    | h::t -> sum' t (h::acc)

let rec concat' l acc = 
    match l with
    | [] -> acc
    | h::t -> concat' t (acc^h)

let fold_left f acc l = 
    match l with
    | [] -> acc
    | h::t -> fold_left f (f a h) t

let fold_right f acc l = 
    match l with
    | [] -> acc
    | h::t -> f h (fold_right f t acc)

(* use List.fold to implement more functions *)
let length lst = List.fold_left (fun acc elem -> acc + 1) 0 lst

let rev lst = List.fold_left (fun acc elem -> elem::acc) [] lst

let map f l = List.fold_right (fun elem acc -> (f elem)::acc) l []