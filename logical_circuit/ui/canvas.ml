open Logic

type tool = Select | Place of Circuit.kind

type selection = Node of Doc.id | Wire of Doc.wire

type drag =
  | Moving of Doc.id * float * float  (** node, and the offset it was grabbed at *)
  | Wiring of Doc.id * float * float  (** source node, and where the free end is now *)

type view = {
  values : Gate.value array;
  on_cycle : bool array;
  letters : string array;
  selection : selection option;
  drag : drag option;
}

let snap v = Float.round (v /. Shapes.cell) *. Shapes.cell

let letter k =
  if k < 26 then String.make 1 (Char.chr (Char.code 'A' + k))
  else Printf.sprintf "in%d" k

let name_of = function Circuit.Input -> "input" | Circuit.Gate g -> Gate.to_string g

(* Everything the drawing needs about the board's state, in one pass over it. *)
let snapshot doc =
  let _, values, cycle = Doc.evaluate doc in
  let count = Doc.count doc in
  let on_cycle = Array.make count false in
  List.iter (fun id -> if id < count then on_cycle.(id) <- true) cycle;
  let letters = Array.make count "" in
  let k = ref 0 in
  Doc.iter doc (fun node ->
      match node.kind with
      | Circuit.Input ->
          letters.(node.id) <- letter !k;
          incr k
      | Circuit.Gate _ -> ());
  (values, on_cycle, letters)

let value_of view id =
  if id >= 0 && id < Array.length view.values then view.values.(id) else Gate.Unknown

let on_cycle view id =
  id >= 0 && id < Array.length view.on_cycle && view.on_cycle.(id)

let wire_endpoints doc (w : Doc.wire) =
  let src = Doc.node doc w.src and dst = Doc.node doc w.dst in
  let a = Shapes.output_port src.kind in
  let b = List.nth (Shapes.input_ports dst.kind) w.port in
  (src.x +. a.x, src.y +. a.y, dst.x +. b.x, dst.y +. b.y)

let cubic x0 y0 x1 y1 x2 y2 x3 y3 t =
  let u = 1. -. t in
  let a = u *. u *. u and b = 3. *. u *. u *. t in
  let c = 3. *. u *. t *. t and d = t *. t *. t in
  ( (a *. x0) +. (b *. x1) +. (c *. x2) +. (d *. x3),
    (a *. y0) +. (b *. y1) +. (c *. y2) +. (d *. y3) )

let draw_wire cr ~color ~width ~dash doc (w : Doc.wire) =
  let sx, sy, dx, dy = wire_endpoints doc w in
  let x0, y0, x1, y1, x2, y2, x3, y3 = Shapes.wire_controls sx sy dx dy in
  Cairo.move_to cr x0 y0;
  Cairo.curve_to cr x1 y1 x2 y2 x3 y3;
  Shapes.set_color cr color;
  Cairo.set_line_width cr width;
  (match dash with Some d -> Cairo.set_dash cr d | None -> ());
  Cairo.stroke cr;
  Cairo.set_dash cr [||]

let render cr doc ~width ~height view =
  Shapes.set_color cr Shapes.paper;
  Cairo.paint cr;
  let x = ref 0. in
  while !x <= width do
    let y = ref 0. in
    while !y <= height do
      Cairo.Path.sub cr;
      Cairo.arc cr !x !y ~r:1.1 ~a1:0. ~a2:(2. *. Float.pi);
      y := !y +. Shapes.cell
    done;
    x := !x +. Shapes.cell
  done;
  Shapes.set_color cr Shapes.grid;
  Cairo.fill cr;
  List.iter
    (fun (w : Doc.wire) ->
      let picked = view.selection = Some (Wire w) in
      let color =
        if on_cycle view w.src && on_cycle view w.dst then Shapes.cycle
        else if picked then Shapes.selected
        else Shapes.color_of_value (value_of view w.src)
      in
      draw_wire cr ~color ~width:(if picked then 3.5 else 2.5) ~dash:None doc w)
    (Doc.wires doc);
  (match view.drag with
  | Some (Wiring (src, mx, my)) ->
      let node = Doc.node doc src in
      let port = Shapes.output_port node.kind in
      let sx = node.x +. port.x and sy = node.y +. port.y in
      let x0, y0, x1, y1, x2, y2, x3, y3 = Shapes.wire_controls sx sy mx my in
      Cairo.move_to cr x0 y0;
      Cairo.curve_to cr x1 y1 x2 y2 x3 y3;
      Shapes.set_color cr Shapes.selected;
      Cairo.set_line_width cr 2.5;
      Cairo.set_dash cr [| 6.; 4. |];
      Cairo.stroke cr;
      Cairo.set_dash cr [||]
  | Some (Moving _) | None -> ());
  Doc.iter doc (fun node ->
      (match node.kind with
      | Circuit.Input ->
          Shapes.draw_input cr ~x:node.x ~y:node.y ~label:view.letters.(node.id)
            ~value:(if node.level then Gate.High else Gate.Low)
      | Circuit.Gate g -> Shapes.draw_gate cr ~x:node.x ~y:node.y g);
      List.iteri
        (fun i (port : Shapes.point) ->
          let px = node.x +. port.x and py = node.y +. port.y in
          (match node.ports.(i) with
          | Some src ->
              Cairo.arc cr px py ~r:Shapes.port_radius ~a1:0. ~a2:(2. *. Float.pi);
              Shapes.set_color cr (Shapes.color_of_value (value_of view src));
              Cairo.fill cr
          | None -> ());
          Cairo.arc cr px py ~r:Shapes.port_radius ~a1:0. ~a2:(2. *. Float.pi);
          Shapes.set_color cr Shapes.ink;
          Cairo.set_line_width cr 1.5;
          Cairo.stroke cr)
        (Shapes.input_ports node.kind);
      let out = Shapes.output_port node.kind in
      Cairo.arc cr (node.x +. out.x) (node.y +. out.y) ~r:Shapes.port_radius ~a1:0.
        ~a2:(2. *. Float.pi);
      Shapes.set_color cr (Shapes.color_of_value (value_of view node.id));
      Cairo.fill cr;
      Cairo.arc cr (node.x +. out.x) (node.y +. out.y) ~r:Shapes.port_radius ~a1:0.
        ~a2:(2. *. Float.pi);
      Shapes.set_color cr Shapes.ink;
      Cairo.set_line_width cr 1.5;
      Cairo.stroke cr;
      if view.selection = Some (Node node.id) then begin
        Cairo.rectangle cr (node.x -. 6.) (node.y -. 6.) ~w:(Shapes.size +. 12.)
          ~h:(Shapes.size +. 12.);
        Shapes.set_color cr Shapes.selected;
        Cairo.set_line_width cr 2.;
        Cairo.set_dash cr [| 5.; 3. |];
        Cairo.stroke cr;
        Cairo.set_dash cr [||]
      end)

let render_board cr doc ~width ~height =
  let values, on_cycle, letters = snapshot doc in
  render cr doc ~width ~height
    { values; on_cycle; letters; selection = None; drag = None }

type port_hit = Out of Doc.id | In of Doc.id * int

class canvas () = object (self)
  val doc = Doc.create ()
  val area = GMisc.drawing_area ()
  val mutable values : Gate.value array = [||]
  val mutable on_cycle : bool array = [||]
  val mutable letters : string array = [||]
  val mutable tool = Select
  val mutable selection : selection option = None
  val mutable drag : drag option = None
  val mutable moved = false
  val mutable report : string -> unit = (fun _ -> ())
  val mutable tool_used : tool -> unit = (fun _ -> ())

  method widget = (area :> GObj.widget)
  method board = doc

  method set_status f = report <- f
  method set_tool_callback f = tool_used <- f

  method set_tool t =
    if tool <> t then begin
      tool <- t;
      tool_used t
    end

  method private refresh () =
    let fresh, cycle, names = snapshot doc in
    values <- fresh;
    on_cycle <- cycle;
    letters <- names;
    area#misc#queue_draw ()

  method private view () = { values; on_cycle; letters; selection; drag }

  method private node_at mx my =
    let hit = ref None in
    Doc.iter doc (fun node ->
        if
          mx >= node.x && mx <= node.x +. Shapes.size && my >= node.y
          && my <= node.y +. Shapes.size
        then hit := Some node.id);
    !hit

  method private port_at mx my =
    let near x y = Float.hypot (mx -. x) (my -. y) <= 9. in
    let hit = ref None in
    Doc.iter doc (fun node ->
        let out = Shapes.output_port node.kind in
        if near (node.x +. out.x) (node.y +. out.y) then hit := Some (Out node.id)
        else
          List.iteri
            (fun i (port : Shapes.point) ->
              if near (node.x +. port.x) (node.y +. port.y) then
                hit := Some (In (node.id, i)))
            (Shapes.input_ports node.kind));
    !hit

  method private wire_at mx my =
    let best = ref (None, 8.) in
    List.iter
      (fun (w : Doc.wire) ->
        let sx, sy, dx, dy = wire_endpoints doc w in
        let x0, y0, x1, y1, x2, y2, x3, y3 = Shapes.wire_controls sx sy dx dy in
        for step = 0 to 20 do
          let px, py = cubic x0 y0 x1 y1 x2 y2 x3 y3 (float step /. 20.) in
          let distance = Float.hypot (mx -. px) (my -. py) in
          let _, shortest = !best in
          if distance < shortest then best := (Some w, distance)
        done)
      (Doc.wires doc);
    fst !best

  method delete_selection () =
    (match selection with
    | Some (Node id) ->
        Doc.remove doc id;
        report "node deleted"
    | Some (Wire w) ->
        Doc.disconnect doc ~dst:w.dst ~port:w.port;
        report "wire deleted"
    | None -> report "nothing is selected");
    selection <- None;
    self#refresh ()

  method clear () =
    Doc.clear doc;
    selection <- None;
    drag <- None;
    report "the board is empty";
    self#refresh ()

  method private press ev =
    let mx = GdkEvent.Button.x ev and my = GdkEvent.Button.y ev in
    area#misc#grab_focus ();
    (match GdkEvent.Button.button ev with
    | 1 -> (
        match tool with
        | Place kind ->
            let id = Doc.add doc ~kind ~x:(snap mx) ~y:(snap my) in
            selection <- Some (Node id);
            report (name_of kind ^ " added");
            self#set_tool Select;
            self#refresh ()
        | Select -> (
            match self#port_at mx my with
            | Some (Out id) -> drag <- Some (Wiring (id, mx, my))
            | Some (In (dst, port)) ->
                if (Doc.node doc dst).ports.(port) <> None then begin
                  Doc.disconnect doc ~dst ~port;
                  report "wire removed";
                  self#refresh ()
                end
            | None -> (
                match self#node_at mx my with
                | Some id ->
                    let node = Doc.node doc id in
                    drag <- Some (Moving (id, mx -. node.x, my -. node.y))
                | None -> (
                    match self#wire_at mx my with
                    | Some w ->
                        selection <- Some (Wire w);
                        area#misc#queue_draw ()
                    | None ->
                        selection <- None;
                        area#misc#queue_draw ()))))
    | 3 -> (
        match self#node_at mx my with
        | Some id ->
            Doc.remove doc id;
            selection <- None;
            report "node deleted";
            self#refresh ()
        | None -> (
            match self#wire_at mx my with
            | Some w ->
                Doc.disconnect doc ~dst:w.dst ~port:w.port;
                selection <- None;
                report "wire deleted";
                self#refresh ()
            | None -> ()))
    | _ -> ());
    true

  method private motion ev =
    let mx = GdkEvent.Motion.x ev and my = GdkEvent.Motion.y ev in
    (match drag with
    | Some (Moving (id, ox, oy)) ->
        Doc.move doc id ~x:(snap (mx -. ox)) ~y:(snap (my -. oy));
        moved <- true;
        area#misc#queue_draw ()
    | Some (Wiring (src, _, _)) ->
        drag <- Some (Wiring (src, mx, my));
        moved <- true;
        area#misc#queue_draw ()
    | None -> ());
    true

  method private release ev =
    let mx = GdkEvent.Button.x ev and my = GdkEvent.Button.y ev in
    (match drag with
    | Some (Moving (id, _, _)) when not moved -> (
        match (Doc.node doc id).kind with
        | Circuit.Input ->
            Doc.toggle doc id;
            report "input toggled";
            self#refresh ()
        | Circuit.Gate _ -> ())
    | Some (Wiring (src, _, _)) -> (
        match self#port_at mx my with
        | Some (In (dst, port)) -> (
            match Doc.connect ~port doc ~src ~dst with
            | Some _ ->
                report "wire added";
                self#refresh ()
            | None -> report "every input port of that gate is wired already")
        | Some (Out _) | None -> report "a wire has to end on an input port")
    | Some (Moving _) | None -> ());
    drag <- None;
    moved <- false;
    area#misc#queue_draw ();
    true

  method private key ev =
    match GdkEvent.Key.keyval ev with
    | k when k = GdkKeysyms._Delete || k = GdkKeysyms._BackSpace ->
        self#delete_selection ();
        true
    | k when k = GdkKeysyms._Escape ->
        drag <- None;
        self#set_tool Select;
        area#misc#queue_draw ();
        true
    | _ -> false

  initializer
    area#misc#set_can_focus true;
    area#set_events
      [ `EXPOSURE; `BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `KEY_PRESS ];
    ignore (area#misc#connect#draw ~callback:(fun cr ->
        let area_size = area#misc#allocation in
        render cr doc ~width:(float area_size.Gtk.width)
          ~height:(float area_size.Gtk.height) (self#view ());
        true));
    ignore (area#event#connect#button_press ~callback:self#press);
    ignore (area#event#connect#button_release ~callback:self#release);
    ignore (area#event#connect#motion_notify ~callback:self#motion);
    ignore (area#event#connect#key_press ~callback:self#key);
    self#refresh ()
end
