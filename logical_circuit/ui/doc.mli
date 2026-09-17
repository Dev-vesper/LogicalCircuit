(** The editable board: nodes with a position and an input level, and the wires between
    their ports. {!build} turns it into the immutable [Logic.Circuit.t] the core
    evaluates; the core circuit is append-only, so the editor's model cannot be the
    core's — this is not a second copy of it, positions and removal live only here. *)

type id = int

type node = {
  mutable id : id;  (** kept dense by {!remove}, which renumbers the nodes after it *)
  kind : Logic.Circuit.kind;
  mutable x : float;
  mutable y : float;
  mutable level : bool;  (** the value of an input node *)
  mutable ports : id option array;  (** the source node of each input port, in port order *)
}

type wire = { src : id; dst : id; port : int }

type t

val create : unit -> t
val count : t -> int

val node : t -> id -> node
(** @raise Invalid_argument if there is no such node. *)

val iter : t -> (node -> unit) -> unit
val wires : t -> wire list
(** Every wire, in node id and then port order. *)

val add : t -> kind:Logic.Circuit.kind -> x:float -> y:float -> id
val move : t -> id -> x:float -> y:float -> unit
val toggle : t -> id -> unit

val connect : ?port:int -> t -> src:id -> dst:id -> int option
(** Wires [src] into [dst], preferring [port] when it is free and taking the first free
    port otherwise. [None] when every port of [dst] is taken. *)

val disconnect : t -> dst:id -> port:int -> unit
val remove : t -> id -> unit
(** Drops the node and every wire that touches it. Ids stay dense: the nodes after it
    move down one, and the wires that pointed at them follow. *)

val clear : t -> unit

val build : t -> Logic.Circuit.t
val evaluate : t -> Logic.Circuit.t * Logic.Gate.value array * id list
(** The circuit, the value of every node indexed by id, and the nodes on a feedback
    cycle if there is one. *)
