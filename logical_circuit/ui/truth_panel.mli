(** The board's truth table in a window of its own. It never writes to the board: it
    builds the immutable circuit, asks the core for the table and, when the board is not
    finished, shows the core's refusal instead. *)

type t

val create : unit -> t
(** Builds the window hidden; {!show} presents it. *)

val show : t -> Doc.t -> unit
