let bar ?packing ~quit ~truth_table ~keys () =
  let menubar = GMenu.menu_bar ?packing () in
  let factory = new GMenu.factory menubar in
  let file = new GMenu.factory (factory#add_submenu "File") in
  ignore (file#add_item "Quit" ~key:GdkKeysyms._Q ~callback:quit);
  let view = new GMenu.factory (factory#add_submenu "View") in
  ignore (view#add_item "Truth table" ~key:GdkKeysyms._T ~callback:truth_table);
  let help = new GMenu.factory (factory#add_submenu "Help") in
  ignore (help#add_item "Keys" ~key:GdkKeysyms._K ~callback:keys);
  menubar
