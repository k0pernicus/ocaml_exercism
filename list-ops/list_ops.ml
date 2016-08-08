let length list =
    let rec aux acc list =
        match list with
        | [] -> acc
        | _ :: t -> aux (acc + 1) t in
    aux 0 list;;

let reverse list =
    let rec aux acc list =
        match list with
        | [] -> acc
        | h :: t -> aux (h :: acc) t in
    aux [] list;;

let map ~f list =
    let rec aux acc list =
        match list with
        | [] -> reverse acc
        | h :: t -> aux ((f h) :: acc) t in
    aux [] list;;

let filter ~f list =
    let rec aux acc list =
        match list with
        | [] -> reverse acc
        | h :: t -> (
            match (f h) with
            | true -> aux (h :: acc) t
            | _ -> aux acc t
        ) in
    aux [] list;;

let rec fold ~init:acc ~f l =
    match l with
    | [] -> acc
    | h :: t -> fold ~init:(f acc h) ~f t;;

let append list1 list2 =
    let rec aux list list2 =
        match list2 with
        | [] -> reverse list
        | h :: t -> aux (h :: list) t in
    aux (reverse list1) list2;;

let concat listOfLists =
    let rec aux acc list =
        match list with
        | [] -> acc
        | h :: t -> aux (append acc h) t in
    aux [] listOfLists;;

