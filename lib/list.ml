(* get lat element of a list *)
let rec last (l: 'a list) : 'a option =
    match l with
    | [] -> None
    | [x] -> Some x
    | _ :: xs -> last xs
;;

(* get last two elements of a list *)
let rec last_two (l: 'a list) : ('a * 'a) option =
    match l with
    | [] -> None
    | [_] -> None
    | [x;y] -> Some (x, y)
    | _ :: xs -> last_two xs
;;

(* get nth element of a list *)
let rec nth (l: 'a list) (k: int) : 'a option =
    match l with
    | [] -> None
    | x :: xs -> if k == 0 then Some x else nth xs (k-1)
;;

(* calculate length of the list *)
let len (l: 'a list) : int =
    let rec list_len' (l: 'a list) (i: int) : int =
        match l with
        | [] -> i
        | _ :: xs -> list_len' xs (i+1)
    in
    list_len' l 0
;;

(* reverse a list *)
let reverse (l: 'a list) : 'a list =
    let rec reverse' (l: 'a list) (acc: 'a list) : 'a list =
        match l with
        | [] -> acc
        | x :: xs -> reverse' xs (x :: acc)
    in
    reverse' l []
;;

(* TODO: solve is_palindrome *)
let is_palindrome (_l: 'a list) : bool =
    false
;;

(* tail recursive map *)
(** This version of map function is optimized (O n) but the result is in reversed order
    If you want the right order, call reverse (O n^2) *)
let map (f: ('a -> 'b)) (l: 'a list) : 'b list =
    let rec map' (f: ('a -> 'b)) (l: 'a list) (acc: 'b list) : 'b list =
        match l with
        | [] -> acc
        | x :: xs -> map' f xs (f x :: acc)
    in
    map' f l []
;;

(* tail recursive filter *)
(** This version of filter function is optimized (O n) but the result is in reversed order
    If you want the right order, call reverse (O n^2) *)
let filter (f: ('a -> bool)) (l: 'a list) : 'a list =
    let rec filter' (f: ('a -> bool)) (l: 'a list) (acc: 'a list) : 'a list =
        match l with
        | [] -> acc
        | x :: xs -> if (f x) then filter' f xs (x :: acc) else filter' f xs acc
    in
    filter' f l []
;;

(* tail recursive sum *)
let sum (l: int list) : int =
    let rec sum' (l: int list) (acc: int) : int =
        match l with
        | [] -> acc
        | x :: xs -> sum' xs (acc + x)
    in
    sum' l 0
;;

let range (l: int) (h: int) : int list =
    let rec range' (l: int) (h: int) (acc: int list) : int list =
        if h <= l then
            acc
        else
            range' l (h-1) (h :: acc)
    in
    range' l h []
;;
