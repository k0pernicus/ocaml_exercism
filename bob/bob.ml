open Core.Std

let is_empty s = String.for_all s ~f:Char.is_whitespace

let is_uppercase_n_alpha c =
    not (Char.is_alpha c) || (Char.is_uppercase c)

let is_shouting s =
    String.exists ~f:Char.is_alpha s &&
    String.for_all ~f:is_uppercase_n_alpha s

let is_question s = String.is_suffix s ~suffix:"?"

let response_for = function
    | s when is_empty s -> "Fine. Be that way!"
    | s when is_shouting s -> "Whoa, chill out!"
    | s when is_question s -> "Sure."
    | _ -> "Whatever."
