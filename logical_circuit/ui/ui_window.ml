let keys_text =
  String.concat "\n"
    [
      "Left click           arm a tool, then place it; otherwise select, or start";
      "                     dragging the node under the cursor";
      "Drag from a port     draw a wire, released on a free input port";
      "Click on an input    toggle it between 0 and 1";
      "Click on a wire      select it";
      "Right click          delete the node or the wire under the cursor";
      "Delete, Backspace    delete the selection";
      "Escape               drop the drag and go back to Select";
      "";
      "Green cable = 1, grey = 0, amber = no value yet, red = feedback cycle.";
    ]

let create () =
  let window = GWindow.window ~title:"Logical circuit" ~width:960 ~height:680 () in
  let vbox = GPack.vbox ~packing:window#add () in
  let canvas = new Canvas.canvas () in
  let truth = Truth_panel.create () in
  let show_truth () = Truth_panel.show truth canvas#board in
  ignore
    (Menu.bar
       ~packing:(fun w -> vbox#pack ~fill:false w)
       ~quit:(fun () -> window#destroy ())
       ~truth_table:show_truth
       ~keys:(fun () -> GToolbox.message_box ~parent:window ~title:"Keys" keys_text)
       ());
  let toolbar = Buttons.create canvas ~on_truth_table:show_truth in
  vbox#pack ~fill:false (Buttons.widget toolbar);
  vbox#pack ~expand:true canvas#widget;
  let status = GMisc.label ~text:"place a gate or an input to begin" ~xalign:0. () in
  vbox#pack ~fill:false status#coerce;
  canvas#set_status (fun message -> status#set_text message);
  canvas#set_tool_callback (fun tool -> Buttons.select toolbar tool);
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  window#misc#show_all ();
  window
