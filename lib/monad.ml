module type Monad = sig
    type 'a t
    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val unwrap : 'a t -> 'a
end

module Maybe = struct
    type 'a t = | None | Some of 'a

    let return (x: 'a) : 'a t = Some x;;

    let some (x: 'a) : 'a t = return x;;

    let none : 'a t = None;;

    let ( >>= ) (m: 'a t) (f: 'a -> 'b t) : 'b t =
        match m with
        | None -> None
        | Some v -> f v
    ;;

    let bind (m: 'a t) (f: 'a -> 'b t) : 'b t = (>>=) m f;;

    let unwrap (x: 'a t) : 'a =
        match x with
        | None -> failwith "called unwrap on None"
        | Some v -> v
    ;;

    let unwrap_or (x: 'a t) (default: 'a) : 'a =
        match x with
        | None -> default
        | Some v -> v
    ;;
end
