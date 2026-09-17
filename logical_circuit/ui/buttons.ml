open Logic

type t = { box : GPack.box; buttons : (Canvas.tool * GButton.radio_button) list }

let create (canvas : Canvas.canvas) ~on_truth_table =
  let box = GPack.hbox ~spacing:6 ~border_width:6 () in
  let buttons = ref [] in
  let group = ref None in
  let arm tool label =
    let button = GButton.radio_button ?group:(!group) ~label ~packing:box#pack () in
    (match !group with None -> group := Some button#group | Some _ -> ());
    buttons := (tool, button) :: !buttons;
    ignore
      (button#connect#toggled ~callback:(fun () ->
           if button#active then canvas#set_tool tool))
  in
  arm Canvas.Select "Select";
  arm (Canvas.Place (Circuit.Gate Gate.And)) "AND";
  arm (Canvas.Place (Circuit.Gate Gate.Or)) "OR";
  arm (Canvas.Place (Circuit.Gate Gate.Not)) "NOT";
  arm (Canvas.Place Circuit.Input) "Input";
  let action label callback =
    let button = GButton.button ~label ~packing:box#pack () in
    ignore (button#connect#clicked ~callback)
  in
  action "Delete" (fun () -> canvas#delete_selection ());
  action "Clear" (fun () -> canvas#clear ());
  action "Truth table" on_truth_table;
  { box; buttons = !buttons }

let widget t = t.box#coerce

let select t tool =
  match List.assoc_opt tool t.buttons with
  | Some button -> button#set_active true
  | None -> ()
