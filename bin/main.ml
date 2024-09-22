let () =
    let xs = Practice.List.range 1 10 in
    Printf.printf "sum = %d\n" (Practice.List.sum xs);
    Printf.printf "sum of evens = %d\n" (xs |> Practice.List.filter (fun x -> x mod 2 = 0) |> Practice.List.sum);
    Printf.printf "first element of xs = %s\n" 
        (match (Practice.List.nth xs 0) with
            | Some x -> string_of_int x
            | None -> "not found");
    Printf.printf "length of xs = %d\n" (Practice.List.len xs);
    Printf.printf "sum of squared = %d\n" (xs |> Practice.List.map (fun x -> x * x) |> Practice.List.sum);
;;
