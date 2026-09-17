type t = And | Or | Not

type value = Low | High | Unknown

let arity = function And | Or -> 2 | Not -> 1

let of_bool b = if b then High else Low

let to_bool = function
  | High -> true
  | Low -> false
  | Unknown -> invalid_arg "Logic.Gate.to_bool: the value is unknown"

let eval3 gate inputs =
  match gate with
  | And ->
      if List.exists (fun v -> v = Low) inputs then Low
      else if List.for_all (fun v -> v = High) inputs then High
      else Unknown
  | Or ->
      if List.exists (fun v -> v = High) inputs then High
      else if List.for_all (fun v -> v = Low) inputs then Low
      else Unknown
  | Not -> (
      match inputs with
      | [ Low ] -> High
      | [ High ] -> Low
      | [ Unknown ] -> Unknown
      | _ -> invalid_arg "Logic.Gate.eval: Not expects exactly one input")

let eval gate inputs = to_bool (eval3 gate (List.map of_bool inputs))

let to_string = function And -> "AND" | Or -> "OR" | Not -> "NOT"
