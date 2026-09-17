open Logic

type tool = Select | Place of Circuit.kind

type selection = Node of Doc.id | Wire of Doc.wire

type drag =
  | Moving of { node : Doc.id; offset : float * float; origin : float * float }
      (** node, the offset it was grabbed at, and where the press landed *)
  | Wiring of { src : Doc.id; tx : float; ty : float; target : (Doc.id * int) option }
      (** source node, where the free end is now, and the input port it settled on *)
  | Adjusting of {
      wire : Doc.wire;
      segment : int;
      vertical : bool;
      grab : float;  (** cursor minus the segment, along the axis it moves on *)
      origin : float * float;  (** where the press landed, to tell a click from a drag *)
      bends : (float * float) list ref;  (** the working copy of the wire's bends *)
    }
  | Shaping of {
      wire : Doc.wire;
      index : int;  (** which bend of the wire follows the pointer *)
      origin : float * float;
      bends : (float * float) list;  (** the wire's bends as the press found them *)
    }

type view = {
  values : Gate.value array;
  on_cycle : bool array;
  letters : string array;
  selection : selection option;
  hover : Doc.wire option;
  drag : drag option;
}

let snap v = Float.round (v /. Shapes.cell) *. Shapes.cell

let snap_radius = 14.
let wire_hit_radius = 8.
let handle_radius = 8.

(* The dot lattice is painted twice as densely as the grid things snap to: it is only the
   page's texture, while positions stay on [Shapes.cell]. The whole board is painted with
   one repeating pattern, instead of one dot per arc, per expose. *)
let dot_spacing = Shapes.cell /. 2.

(* A point of a wire moves from dot to dot, one at a time. *)
let dot_snap v = Float.round (v /. dot_spacing) *. dot_spacing

let grid_tile =
  lazy
    (let side = int_of_float dot_spacing in
     let tile = Cairo.Image.create Cairo.Image.ARGB32 ~w:side ~h:side in
     let cr = Cairo.create tile in
     Cairo.arc cr 0. 0. ~r:1. ~a1:0. ~a2:(2. *. Float.pi);
     Shapes.set_color cr Shapes.grid;
     Cairo.fill cr;
     tile)

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

let wire_ends doc (w : Doc.wire) =
  let src = Doc.node doc w.src and dst = Doc.node doc w.dst in
  let a = Shapes.output_port src.kind in
  let b = List.nth (Shapes.input_ports dst.kind) w.port in
  (src.x +. a.x, src.y +. a.y, dst.x +. b.x, dst.y +. b.y)

(* The bends the wire is drawn with: the ones the user adjusted, or the ones the router
   would lay down now. *)
let bends_of doc (w : Doc.wire) =
  match Doc.route doc ~dst:w.dst ~port:w.port with
  | [] ->
      let sx, sy, dx, dy = wire_ends doc w in
      List.map (fun (p : Shapes.point) -> (p.x, p.y)) (Shapes.route ~sx ~sy ~dx ~dy)
  | stored -> stored

let wire_path doc (w : Doc.wire) =
  let sx, sy, dx, dy = wire_ends doc w in
  (sx, sy) :: (bends_of doc w @ [ (dx, dy) ])

let distance_to_segment px py ax ay bx by =
  let vx = bx -. ax and vy = by -. ay in
  let length2 = (vx *. vx) +. (vy *. vy) in
  let t =
    if length2 = 0. then 0.
    else
      Float.max 0. (Float.min 1. (((px -. ax) *. vx) +. ((py -. ay) *. vy) /. length2))
  in
  Float.hypot (px -. (ax +. (t *. vx))) (py -. (ay +. (t *. vy)))

(* Collapses what a drag can leave behind: repeated points, and turns through a middle
   point that lies on the straight line between its neighbours. *)
let normalize bends =
  let rec go = function
    | (x0, y0) :: (x1, y1) :: rest when x0 = x1 && y0 = y1 -> go ((x1, y1) :: rest)
    | (x0, y0) :: (x1, y1) :: (x2, y2) :: rest
      when (x0 = x1 && x1 = x2) || (y0 = y1 && y1 = y2) ->
        go ((x0, y0) :: (x2, y2) :: rest)
    | p :: rest -> p :: go rest
    | [] -> []
  in
  go bends

(* A single bend dragged off its row or column leaves its two segments diagonal; a corner
   is put back before each of them, so the path — the moved bend, the corners it now needs,
   and every point the user did not touch — keeps its right angles. *)
let orthogonalize pts =
  let rec go acc prev = function
    | p :: rest when p = prev -> go acc prev rest
    | p :: rest ->
        let acc =
          if fst prev = fst p || snd prev = snd p then acc
          else
            let corner =
              match rest with
              | q :: _ when fst p = fst q -> (fst p, snd prev)
              | _ -> (fst prev, snd p)
            in
            corner :: acc
        in
        go (p :: acc) p rest
    | [] -> List.rev acc
  in
  match pts with [] | [ _ ] -> pts | p :: rest -> go [ p ] p rest

let with_bend bends index (x, y) =
  orthogonalize (List.mapi (fun i p -> if i = index then (x, y) else p) bends)

let draw_path cr ~color ~width ~dash pts =
  match pts with
  | (x0, y0) :: rest ->
      Cairo.move_to cr x0 y0;
      List.iter (fun (x, y) -> Cairo.line_to cr x y) rest;
      Shapes.set_color cr color;
      Cairo.set_line_width cr width;
      Cairo.set_line_join cr Cairo.JOIN_ROUND;
      (match dash with Some d -> Cairo.set_dash cr d | None -> ());
      Cairo.stroke cr;
      Cairo.set_dash cr [||]
  | [] -> ()

let render cr doc ~width ~height view =
  Shapes.set_color cr Shapes.paper;
  Cairo.paint cr;
  let tile = Cairo.Pattern.create_for_surface (Lazy.force grid_tile) in
  Cairo.Pattern.set_extend tile Cairo.Pattern.REPEAT;
  Cairo.set_source cr tile;
  Cairo.rectangle cr 0. 0. ~w:width ~h:height;
  Cairo.fill cr;
  List.iter
    (fun (w : Doc.wire) ->
      let picked = view.selection = Some (Wire w) in
      let hovered = view.hover = Some w in
      let color =
        if on_cycle view w.src && on_cycle view w.dst then Shapes.cycle
        else if picked || hovered then Shapes.selected
        else Shapes.color_of_value (value_of view w.src)
      in
      draw_path cr ~color ~width:(if picked then 3.5 else if hovered then 3. else 2.5)
        ~dash:None (wire_path doc w))
    (Doc.wires doc);
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
      end);
  (match (match view.selection with Some (Wire w) -> Some w | _ -> view.hover) with
  | Some w ->
      List.iter
        (fun (x, y) ->
          Cairo.rectangle cr (x -. 3.) (y -. 3.) ~w:6. ~h:6.;
          Shapes.set_color cr Shapes.paper;
          Cairo.fill_preserve cr;
          Shapes.set_color cr Shapes.selected;
          Cairo.set_line_width cr 1.5;
          Cairo.stroke cr)
        (bends_of doc w)
  | None -> ());
  match view.drag with
  | Some (Wiring d) ->
      let node = Doc.node doc d.src in
      let port = Shapes.output_port node.kind in
      let sx = node.x +. port.x and sy = node.y +. port.y in
      let bends =
        List.map
          (fun (p : Shapes.point) -> (p.x, p.y))
          (Shapes.route ~sx ~sy ~dx:d.tx ~dy:d.ty)
      in
      draw_path cr ~color:Shapes.selected ~width:2.5 ~dash:(Some [| 6.; 4. |])
        ((sx, sy) :: (bends @ [ (d.tx, d.ty) ]));
      (match d.target with
      | Some (dst, index) ->
          let target = Doc.node doc dst in
          let p = List.nth (Shapes.input_ports target.kind) index in
          Cairo.arc cr (target.x +. p.x) (target.y +. p.y) ~r:8. ~a1:0.
            ~a2:(2. *. Float.pi);
          Shapes.set_color cr Shapes.selected;
          Cairo.set_line_width cr 2.;
          Cairo.stroke cr
      | None ->
          Cairo.arc cr d.tx d.ty ~r:3. ~a1:0. ~a2:(2. *. Float.pi);
          Shapes.set_color cr Shapes.selected;
          Cairo.fill cr)
  | Some (Moving _) | Some (Adjusting _) | Some (Shaping _) | None -> ()

let render_board cr doc ~width ~height =
  let values, on_cycle, letters = snapshot doc in
  render cr doc ~width ~height
    { values; on_cycle; letters; selection = None; hover = None; drag = None }

type port_hit = Out of Doc.id | In of Doc.id * int

class canvas () = object (self)
  val doc = Doc.create ()
  val area = GMisc.drawing_area ()
  val mutable values : Gate.value array = [||]
  val mutable on_cycle : bool array = [||]
  val mutable letters : string array = [||]
  val mutable tool = Select
  val mutable selection : selection option = None
  val mutable hover : Doc.wire option = None
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

  method private view () = { values; on_cycle; letters; selection; hover; drag }

  method private node_at mx my =
    let hit = ref None in
    Doc.iter doc (fun node ->
        if
          mx >= node.x && mx <= node.x +. Shapes.size && my >= node.y
          && my <= node.y +. Shapes.size
        then hit := Some node.id);
    !hit

  method private port_at mx my =
    let best = ref None in
    let consider x y hit =
      let d = Float.hypot (mx -. x) (my -. y) in
      let better = match !best with None -> true | Some (_, bd) -> d < bd in
      if better && d <= snap_radius then best := Some (hit, d)
    in
    Doc.iter doc (fun node ->
        let out = Shapes.output_port node.kind in
        consider (node.x +. out.x) (node.y +. out.y) (Out node.id);
        List.iteri
          (fun i (port : Shapes.point) ->
            consider (node.x +. port.x) (node.y +. port.y) (In (node.id, i)))
          (Shapes.input_ports node.kind));
    Option.map fst !best

  (* The input port the free end of a wire would settle on, with its position. *)
  method private input_target mx my =
    let best = ref None in
    let consider x y hit =
      let d = Float.hypot (mx -. x) (my -. y) in
      let better = match !best with None -> true | Some (_, _, _, bd) -> d < bd in
      if better && d <= snap_radius then best := Some (hit, x, y, d)
    in
    Doc.iter doc (fun node ->
        List.iteri
          (fun i (port : Shapes.point) ->
            consider (node.x +. port.x) (node.y +. port.y) (node.id, i))
          (Shapes.input_ports node.kind));
    Option.map (fun (hit, x, y, _) -> (hit, x, y)) !best

  method private wire_target mx my =
    match self#input_target mx my with
    | Some (hit, x, y) -> (x, y, Some hit)
    | None -> (dot_snap mx, dot_snap my, None)

  (* The bend handle under the pointer, on any wire: grabbing a point of a wire is how the
     user takes one over. *)
  method private bend_at mx my =
    let best = ref None in
    List.iter
      (fun (w : Doc.wire) ->
        List.iteri
          (fun i (x, y) ->
            let d = Float.hypot (mx -. x) (my -. y) in
            match !best with
            | Some (_, _, bd) when bd <= d -> ()
            | _ -> if d <= handle_radius then best := Some (w, i, d))
          (bends_of doc w))
      (Doc.wires doc);
    match !best with Some (w, i, _) -> Some (w, i) | None -> None

  method private wire_at mx my =
    let best = ref None in
    List.iter
      (fun (w : Doc.wire) ->
        let rec go i = function
          | (ax, ay) :: ((bx, by) :: _ as rest) ->
              let d = distance_to_segment mx my ax ay bx by in
              (match !best with
              | Some (_, _, bd) when bd <= d -> ()
              | _ -> if d <= wire_hit_radius then best := Some (w, i, d));
              go (i + 1) rest
          | _ -> ()
        in
        go 0 (wire_path doc w))
      (Doc.wires doc);
    match !best with Some (w, i, _) -> Some (w, i) | None -> None

  (* How the segment under the press could move, if it can: a vertical segment slides
     sideways, a horizontal one only when both of its ends are bends, because a port does
     not move. *)
  method private segment_handle (w : Doc.wire) segment mx my =
    let bends = bends_of doc w in
    let count = List.length bends in
    let sx, sy, dx, dy = wire_ends doc w in
    let point_at k =
      if k = 0 then (sx, sy)
      else if k = count + 1 then (dx, dy)
      else List.nth bends (k - 1)
    in
    if segment < 0 || segment > count then None
    else
      let ax, ay = point_at segment in
      let bx, by = point_at (segment + 1) in
      let vertical = ax = bx in
      if (not vertical) && not (segment > 0 && segment < count) then None
      else Some (vertical, if vertical then mx -. ax else my -. ay)

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
    hover <- None;
    self#refresh ()

  method clear () =
    Doc.clear doc;
    selection <- None;
    hover <- None;
    drag <- None;
    report "the board is empty";
    self#refresh ()

  method private press ev =
    let mx = GdkEvent.Button.x ev and my = GdkEvent.Button.y ev in
    area#misc#grab_focus ();
    hover <- None;
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
            | Some (Out id) ->
                let tx, ty, target = self#wire_target mx my in
                drag <- Some (Wiring { src = id; tx; ty; target })
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
                    drag <-
                      Some
                        (Moving
                           {
                             node = id;
                             offset = (mx -. node.x, my -. node.y);
                             origin = (mx, my);
                           })
                | None -> (
                    match self#bend_at mx my with
                    | Some (w, index) ->
                        selection <- Some (Wire w);
                        drag <-
                          Some
                            (Shaping
                               {
                                 wire = w;
                                 index;
                                 origin = (mx, my);
                                 bends = bends_of doc w;
                               });
                        area#misc#queue_draw ()
                    | None -> (
                        match self#wire_at mx my with
                        | Some (w, segment) ->
                            selection <- Some (Wire w);
                            (match self#segment_handle w segment mx my with
                            | Some (vertical, grab) ->
                                drag <-
                                  Some
                                    (Adjusting
                                       {
                                         wire = w;
                                         segment;
                                         vertical;
                                         grab;
                                         origin = (mx, my);
                                         bends = ref (bends_of doc w);
                                       })
                            | None -> ());
                            area#misc#queue_draw ()
                        | None ->
                            selection <- None;
                            area#misc#queue_draw ())))))
    | 3 -> (
        match self#node_at mx my with
        | Some id ->
            Doc.remove doc id;
            selection <- None;
            report "node deleted";
            self#refresh ()
        | None -> (
            match self#wire_at mx my with
            | Some (w, _) ->
                Doc.disconnect doc ~dst:w.dst ~port:w.port;
                selection <- None;
                report "wire deleted";
                self#refresh ()
            | None -> ()))
    | _ -> ());
    true

  method private motion ev =
    let mx = GdkEvent.Motion.x ev and my = GdkEvent.Motion.y ev in
    let dragged_far ox oy = Float.hypot (mx -. ox) (my -. oy) >= 4. in
    (match drag with
    | Some (Moving d) ->
        let ox, oy = d.offset in
        Doc.move doc d.node ~x:(snap (mx -. ox)) ~y:(snap (my -. oy));
        if dragged_far (fst d.origin) (snd d.origin) then moved <- true;
        area#misc#queue_draw ()
    | Some (Wiring d) ->
        let tx, ty, target = self#wire_target mx my in
        drag <- Some (Wiring { d with tx; ty; target });
        area#misc#queue_draw ()
    | Some (Adjusting d) ->
        if dragged_far (fst d.origin) (snd d.origin) then begin
          let value = dot_snap (if d.vertical then mx -. d.grab else my -. d.grab) in
          let bends =
            List.mapi
              (fun i (x, y) ->
                if i = d.segment - 1 || i = d.segment then
                  if d.vertical then (value, y) else (x, value)
                else (x, y))
              !(d.bends)
          in
          d.bends := bends;
          Doc.set_route doc ~dst:d.wire.dst ~port:d.wire.port bends;
          moved <- true;
          area#misc#queue_draw ()
        end
    | Some (Shaping d) ->
        if dragged_far (fst d.origin) (snd d.origin) then begin
          let bends = with_bend d.bends d.index (dot_snap mx, dot_snap my) in
          Doc.set_route doc ~dst:d.wire.dst ~port:d.wire.port bends;
          moved <- true;
          area#misc#queue_draw ()
        end
    | None ->
        let under = Option.map fst (self#wire_at mx my) in
        if under <> hover then begin
          hover <- under;
          area#misc#queue_draw ()
        end);
    true

  method private release ev =
    let mx = GdkEvent.Button.x ev and my = GdkEvent.Button.y ev in
    (match drag with
    | Some (Moving d) when not moved -> (
        match (Doc.node doc d.node).kind with
        | Circuit.Input ->
            Doc.toggle doc d.node;
            report "input toggled";
            self#refresh ()
        | Circuit.Gate _ -> ())
    | Some (Wiring d) -> (
        match d.target with
        | Some (dst, port) -> (
            match Doc.connect ~port doc ~src:d.src ~dst with
            | Some _ ->
                report "wire added";
                self#refresh ()
            | None -> report "every input port of that gate is wired already")
        | None -> report "a wire has to end on an input port")
    | Some (Adjusting d) ->
        if moved then begin
          let bends = normalize !(d.bends) in
          Doc.set_route doc ~dst:d.wire.dst ~port:d.wire.port bends;
          report "wire adjusted"
        end
    | Some (Shaping d) ->
        if moved then begin
          let bends = normalize (with_bend d.bends d.index (dot_snap mx, dot_snap my)) in
          Doc.set_route doc ~dst:d.wire.dst ~port:d.wire.port bends;
          report "wire adjusted"
        end
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
