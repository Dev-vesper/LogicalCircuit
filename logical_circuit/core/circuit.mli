(** A combinational circuit: a directed acyclic graph of settable inputs and gates,
    connected by wires. Node ids are dense ints in creation order, so a node id is a
    valid index into the arrays {!simulate} returns. A node may feed several gates
    (fan-out). Feedback is illegal and reported by {!simulate}.

    Geometry is deliberately absent: positions, sizes and drawing belong to the UI. *)

type node_id = int

type kind = Input | Gate of Gate.t

type error =
  | Unknown_node of node_id
  | Not_a_gate of node_id  (** a wire was aimed at an input node *)
  | Not_an_input of node_id  (** an input value was written to a gate node *)
  | Arity_exceeded of node_id  (** the wire would exceed the node's port capacity *)
  | Incomplete of node_id  (** the gate still has unwired input ports *)
  | Cyclic of node_id list  (** the nodes on the feedback cycle, in wire order *)

exception Invalid of error
(** Raised by the accessors and mutators for ids and ports already known to be valid:
    {!eval}, {!wires}, {!successors} and {!connect_exn}. *)

val error_to_string : error -> string

type t

val create : unit -> t
val add_input : t -> bool -> node_id
val add_gate : t -> Gate.t -> node_id
val node_count : t -> int
val kind : t -> node_id -> (kind, error) result
val input_nodes : t -> node_id list
(** Every input node, in ascending id order. This fixes the column order of
    {!Truth_table}. *)

val connect : t -> src:node_id -> dst:node_id -> (unit, error) result
(** Wires [src] into the next free input port of [dst]; port order is the order of the
    [connect] calls. Refuses a wire that would exceed [dst]'s capacity
    ({!Arity_exceeded}) or that targets an input node ({!Not_a_gate}). Connectivity is
    all that is checked here: acyclicity is decided by {!simulate}, the single place
    cycles are detected. *)

val connect_exn : t -> src:node_id -> dst:node_id -> unit
(** [connect] for wiring already known to be valid. @raise Invalid otherwise. *)

val wires : t -> node_id -> node_id list
(** The source node of each input port, in port order. @raise Invalid if the id is
    unknown. *)

val successors : t -> node_id -> node_id list
(** The gates fed by this node (its fan-out), in unspecified order. @raise Invalid if
    the id is unknown. *)

val iter_wires : t -> (node_id -> node_id -> unit) -> unit
(** [iter_wires t f] calls [f src dst] once per wire. *)

val set_input : t -> node_id -> bool -> (unit, error) result
val get_input : t -> node_id -> bool option
(** [None] when the id is unknown or the node is a gate. *)

val validate : t -> (unit, error) result
(** {!simulate} with the values discarded. *)

val simulate_partial : t -> Gate.value array * node_id list
(** The value of every node, indexed by node id, without ever failing. A port with no
    wire drives {!Gate.Unknown}, and so does a gate fed by one; the gates still
    short-circuit on a decisive level, so an [And] with a wired [Low] is [Low] even
    while its other port is empty. The second component lists the nodes of a feedback
    cycle in wire order, or [][] when the circuit is acyclic, so an editor can mark the
    offending wires instead of showing nothing. Costs O(V+E): the same single
    depth-first post-order pass as {!simulate}. *)

val simulate : t -> (bool array, error) result
(** The value of every node of a circuit that is fully wired and acyclic. Reports
    {!Cyclic} if there is a feedback cycle, otherwise {!Incomplete} at a gate with an
    unwired port; when both are present the cycle is reported. *)

val eval : t -> node_id -> bool
(** {!simulate}, then index.
    @raise Invalid if the circuit is invalid or the id unknown. *)
