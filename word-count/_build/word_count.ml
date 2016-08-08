open Core.Std

type mapCount = int String.Map.t

let wordCount mapCount s : int String.Map.t =
    let words = String.split s ' ' and
    count =
        match Map.find mapCount s with
        | None -> 0
        | Some x -> x
    in
    List.iter (Map.add mapCount ~key:(String.lowercase s) ~data:(count + 1)) words;
    mapCount

let word_count s =
    let mapCount = String.Map.empty in
    wordCount mapCount s
