type t = And | Or | Not

let arity = function And | Or -> 2 | Not -> 1

let eval gate inputs =
  match gate with
  | And -> List.for_all Fun.id inputs
  | Or -> List.exists Fun.id inputs
  | Not -> (
      match inputs with
      | [ b ] -> not b
      | _ -> invalid_arg "Logic.Gate.eval: Not expects exactly one input")

let to_string = function And -> "AND" | Or -> "OR" | Not -> "NOT"
