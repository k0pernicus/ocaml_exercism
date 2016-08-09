let rec fold ~init:acc ~f l =
    match l with
    | [] -> acc
    | h :: t -> fold ~init:(f acc h) ~f t;;

let length list =
    fold ~init:0 ~f:(fun acc _ -> acc + 1) list;;

let reverse list =
    fold ~init:[] ~f:(fun acc h -> h :: acc) list;;

let map ~f list =
    fold ~init:[] ~f:(fun acc h -> ((f h) :: acc)) (reverse list);;

let append xs ys =
    fold ~init:ys ~f:(fun acc h -> h :: acc) (reverse xs);;

let concat xss =
    fold ~init:[] ~f:(fun acc h -> append acc h) xss;;

let filter ~f list =
    fold ~init:[] ~f:(fun acc h -> if (f h) then (h :: acc) else acc) (reverse list);;
