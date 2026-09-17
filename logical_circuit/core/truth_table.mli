(** Exhaustive evaluation of a circuit over every combination of its input values. *)

type row = { assignment : bool list; results : bool list }
(** [assignment] follows the column order of {!inputs}, [results] that of {!outputs}. *)

type t = { inputs : Circuit.node_id list; outputs : Circuit.node_id list; rows : row list }

type error =
  | Circuit_error of Circuit.error
  | Too_many_inputs of int

val max_inputs : int
(** Enumeration is exponential, so circuits with more inputs than this are refused. *)

val error_to_string : error -> string

val of_circuit : ?outputs:Circuit.node_id list -> Circuit.t -> (t, error) result
(** Enumerates the assignments in counting order, with the first input as the most
    significant bit, so the first row is all-false and the last is all-true. [outputs]
    defaults to the sink gates: every gate that no other gate consumes. The circuit's
    input values are restored to what they were before the call. *)

val to_string : t -> string
(** An aligned ASCII table, headed by the input letters and the output node ids. *)
