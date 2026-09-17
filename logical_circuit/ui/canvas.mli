(** The board widget: the dot grid, the symbols, the wires and the live values, plus the
    mouse and keyboard gestures that edit the board. It owns the {!Doc.t} it draws, so
    the rest of the window asks the canvas for the board rather than holding one.

    A click never edits the model twice: the press arms a gesture and the release
    finishes it, except for the three edits that a press alone decides (placing a node,
    detaching a wire, deleting in one right-click).

    A wire routes itself at first — a tidy orthogonal path between its two ports — and
    the user can take it over: dragging a segment of a wire moves that segment, with
    every bend landing on the dot grid. While a wire is being pulled out of an output
    port its free end settles on the nearest dot, or snaps onto the input port under the
    pointer. *)

type tool = Select | Place of Logic.Circuit.kind
(** What the next click on the board does: pick something up, or drop a new node. *)

val render_board : Cairo.context -> Doc.t -> width:float -> height:float -> unit
(** Draws a board — dot grid, wires coloured by the value they carry, symbols, ports —
    with nothing selected. The widget draws through this on every expose, and it makes
    the drawing checkable with no display: render into an image surface and write a
    PNG. *)

class canvas : unit -> object
  method widget : GObj.widget
  (** The drawing area, to pack into the window. *)

  method board : Doc.t
  (** The board being edited. Live, not a copy. *)

  method set_status : (string -> unit) -> unit
  (** Where the one-line report of every edit goes. *)

  method set_tool_callback : (tool -> unit) -> unit
  (** Called whenever the armed tool changes, including when dropping a node disarms it,
      so the toolbar can follow. *)

  method set_tool : tool -> unit
  method delete_selection : unit -> unit
  (** Deletes the selected node, with its wires, or the selected wire. Reports when
      nothing is selected. *)

  method clear : unit -> unit
  (** Empties the board. *)
end
