(** The three primary gates, and the single source of truth for gate semantics.
    Nothing else in the library re-implements the logic. *)

type t = And | Or | Not

val arity : t -> int
(** Number of input ports the gate offers for wiring: 2 for [And] and [Or], 1 for [Not]. *)

val eval : t -> bool list -> bool
(** [eval g inputs] applies [g] to [inputs]. [And] and [Or] are n-ary and keep their
    identity element on the empty list (all-true, all-false).
    @raise Invalid_argument if [Not] receives anything but exactly one input. *)

val to_string : t -> string
