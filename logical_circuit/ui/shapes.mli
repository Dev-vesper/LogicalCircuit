(** The geometry and the drawing of the gate symbols, in one place: the canvas and the
    shape preview both draw from here, so a symbol is defined exactly once.

    Every symbol lives in a [size] by [size] box whose top-left corner is the node's
    position, and every port sits on a multiple of [cell] so that wires land on the dot
    grid. *)

val cell : float
(** Spacing of the dot grid; node positions snap to it. *)

val size : float

val input_radius : float
val bubble_radius : float
val port_radius : float

type point = { x : float; y : float }

val input_center : point
(** Centre of the circle an input node is drawn as, relative to its corner. *)

val input_ports : Logic.Circuit.kind -> point list
(** Where a node's input ports are, in port order. An input node offers none. *)

val output_port : Logic.Circuit.kind -> point

val route : sx:float -> sy:float -> dx:float -> dy:float -> point list
(** The interior bends of the nearest orthogonal path from an output port at [(sx, sy)]
    to a target at [(dx, dy)]: no bends when the ports face each other, one dogleg in the
    space between them when there is room for one, and otherwise one dogleg just before
    the target. Every bend is a multiple of [cell]. *)

val ink : float * float * float
val paper : float * float * float
val grid : float * float * float
val selected : float * float * float
val cycle : float * float * float

val color_of_value : Logic.Gate.value -> float * float * float

val set_color : Cairo.context -> float * float * float -> unit

val draw_gate : Cairo.context -> x:float -> y:float -> Logic.Gate.t -> unit
(** Fill and stroke the symbol, with its name below it. *)

val draw_input : Cairo.context -> x:float -> y:float -> label:string -> value:Logic.Gate.value -> unit
