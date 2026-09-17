(** The tool bar: one radio button per tool, so exactly one of them is always armed, and
    the three actions that need no tool. The bar keeps no state of its own — the canvas
    is what remembers the armed tool, and {!select} is how the bar is told to follow
    when the canvas disarms a placement by itself. *)

type t

val create : Canvas.canvas -> on_truth_table:(unit -> unit) -> t

val widget : t -> GObj.widget
(** The bar, to pack into the window. *)

val select : t -> Canvas.tool -> unit
(** Activates the button of [tool]. This asks the canvas to arm it too, which is
    harmless: arming a tool the canvas already holds changes nothing. *)
