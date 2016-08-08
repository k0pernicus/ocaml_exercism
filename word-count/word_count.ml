open Core.Std

let words s = String.split ~on:' ' s

let print_all_map map =
    String.Map.iter map print_map

let print_map ~key ~data =
    print_endline(key ^ " - " ^ string_of_int data)

let wordCount mapCount word : int String.Map.t =
    let count =
        match Map.find mapCount s with
        | None -> 0
        | Some x -> x
    in
    Map.add mapCount ~key:(String.lowercase word) ~data:(count + 1)

let word_count s = words s
    |> List.map ~f:(fun x -> String.lowercase x, 1)
    |> String.Map.of_alist_fold ~init:0 ~f:(+)
