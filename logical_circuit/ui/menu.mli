(** The menu bar. Every item is a window action handed in by the assembler, so the menu
    holds no state and never touches the board itself. *)

val bar :
  ?packing:(GObj.widget -> unit) ->
  quit:(unit -> unit) ->
  truth_table:(unit -> unit) ->
  keys:(unit -> unit) ->
  unit ->
  GMenu.menu_shell
