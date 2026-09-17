open Ui

let () =
  ignore (GtkMain.Main.init ());
  ignore (Ui_window.create ());
  GMain.Main.main ()
