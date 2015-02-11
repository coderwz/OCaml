Ex1:

let rec findLast = 
    (fun lst -> 
     match lst with
     | [] -> None
     | hd::[] -> Some hd
     | hd::tl -> findLast tl)