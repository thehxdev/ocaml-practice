module type Monad = sig
    type 'a t
    val return : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad = struct
    type 'a t = 'a option

    let return (x: 'a) : 'a t = Some x;;

    let ( >>= ) (m: 'a t) (f: 'a -> 'b t) : 'b t =
        match m with
        | None -> None
        | Some v -> f v
    ;;
end
