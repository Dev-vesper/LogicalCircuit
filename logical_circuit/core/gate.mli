(** The three primary gates, and the single source of truth for gate semantics.
    Nothing else in the library re-implements the logic. *)

type t = And | Or | Not

type value = Low | High | Unknown
(** A signal level. [Unknown] is what a port with no wire drives, so a circuit under
    construction still evaluates everywhere else. *)

val arity : t -> int
(** Number of input ports the gate offers for wiring: 2 for [And] and [Or], 1 for [Not]. *)

val eval3 : t -> value list -> value
(** [eval3 g inputs] applies [g] to [inputs], short-circuiting on the decisive level:
    [And] is [Low] as soon as one input is [Low] and [High] only if every input is
    [High]; [Or] is [High] as soon as one input is [High] and [Low] only if every input
    is [Low]; anything else is [Unknown]. [And] and [Or] keep their identity element on
    the empty list (all-true, all-false).
    @raise Invalid_argument if [Not] receives anything but exactly one input. *)

val eval : t -> bool list -> bool
(** [eval g inputs] is {!eval3} on two-valued inputs: it can never be [Unknown].
    @raise Invalid_argument if [Not] receives anything but exactly one input. *)

val of_bool : bool -> value
val to_bool : value -> bool
(** @raise Invalid_argument if the value is [Unknown]. *)

val to_string : t -> string
