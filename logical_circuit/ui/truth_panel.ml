open Logic

type t = { window : GWindow.window; text : GMisc.label }

let create () =
  let window =
    GWindow.window ~title:"Truth table" ~width:380 ~height:460 ~show:false ()
  in
  let scroller =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~packing:window#add ()
  in
  let text =
    GMisc.label ~xalign:0. ~yalign:0. ~selectable:true ~packing:scroller#add ()
  in
  text#misc#modify_font_by_name "monospace 11";
  { window; text }

let show t board =
  t.text#set_text
    (match Truth_table.of_circuit (Doc.build board) with
    | Ok table -> Truth_table.to_string table
    | Error e -> Truth_table.error_to_string e);
  t.window#misc#show_all ()
