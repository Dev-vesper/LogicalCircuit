open Logic

let cell = 20.
let size = 60.
let input_radius = 16.
let bubble_radius = 4.5
let port_radius = 4.5

type point = { x : float; y : float }

let input_center = { x = 30.; y = 30. }

let input_ports = function
  | Circuit.Input -> []
  | Circuit.Gate g -> (
      match g with
      | Gate.And | Gate.Or -> [ { x = 0.; y = 15. }; { x = 0.; y = 45. } ]
      | Gate.Not -> [ { x = 0.; y = 30. } ])

let output_port = function
  | Circuit.Input -> { x = 46.; y = 30. }
  | Circuit.Gate _ -> { x = 60.; y = 30. }

let snap v = Float.round (v /. cell) *. cell

let route ~sx ~sy ~dx ~dy =
  let off = cell in
  if dy = sy && dx >= sx then []
  else if dx >= sx +. (2. *. off) then
    let mid = Float.max (sx +. off) (Float.min (dx -. off) (snap ((sx +. dx) /. 2.))) in
    [ { x = mid; y = sy }; { x = mid; y = dy } ]
  else
    let away = Float.max sy dy +. (2. *. off) in
    [
      { x = sx +. off; y = sy };
      { x = sx +. off; y = away };
      { x = dx -. off; y = away };
      { x = dx -. off; y = dy };
    ]

let ink = (0.13, 0.15, 0.19)
let paper = (1., 1., 1.)
let grid = (0.84, 0.85, 0.88)
let selected = (0.16, 0.44, 0.86)
let high = (0.16, 0.62, 0.31)
let low = (0.45, 0.47, 0.52)
let unknown = (0.88, 0.72, 0.18)
let cycle = (0.85, 0.24, 0.20)

let color_of_value = function Gate.High -> high | Gate.Low -> low | Gate.Unknown -> unknown

let set_color cr (r, g, b) = Cairo.set_source_rgb cr r g b

let centered_text cr ~cx ~cy ~size:font_size ~color text =
  Cairo.select_font_face cr "sans";
  Cairo.set_font_size cr font_size;
  let extents = Cairo.text_extents cr text in
  set_color cr color;
  Cairo.move_to cr (cx -. (extents.Cairo.width /. 2.)) (cy +. (font_size /. 3.));
  Cairo.show_text cr text;
  (* [show_text] leaves the [move_to] current point standing, and an [arc] starting
     there would draw a line from the label to itself. *)
  Cairo.Path.sub cr

let path cr ~x ~y = function
  | Gate.And ->
      Cairo.move_to cr x y;
      Cairo.line_to cr (x +. 30.) y;
      Cairo.arc cr (x +. 30.) (y +. 30.) ~r:30. ~a1:(-.Float.pi /. 2.)
        ~a2:(Float.pi /. 2.);
      Cairo.line_to cr x (y +. 60.);
      Cairo.Path.close cr
  | Gate.Or ->
      Cairo.move_to cr x y;
      Cairo.curve_to cr (x +. 28.) (y +. 2.) (x +. 48.) (y +. 14.) (x +. 60.) (y +. 30.);
      Cairo.curve_to cr (x +. 48.) (y +. 46.) (x +. 28.) (y +. 58.) x (y +. 60.);
      Cairo.curve_to cr (x +. 8.) (y +. 40.) (x +. 8.) (y +. 20.) x y;
      Cairo.Path.close cr
  | Gate.Not ->
      Cairo.move_to cr x y;
      Cairo.line_to cr (x +. 50.) (y +. 30.);
      Cairo.line_to cr x (y +. 60.);
      Cairo.Path.close cr

let fill_and_stroke cr =
  set_color cr paper;
  Cairo.fill_preserve cr;
  set_color cr ink;
  Cairo.set_line_width cr 2.;
  Cairo.stroke cr

let draw_gate cr ~x ~y gate =
  path cr ~x ~y gate;
  fill_and_stroke cr;
  (match gate with
  | Gate.Not ->
      Cairo.arc cr (x +. 55.) (y +. 30.) ~r:bubble_radius ~a1:0.
        ~a2:(2. *. Float.pi);
      fill_and_stroke cr
  | Gate.And | Gate.Or -> ());
  centered_text cr ~cx:(x +. 30.) ~cy:(y +. 74.) ~size:11. ~color:(0.42, 0.45, 0.51)
    (Gate.to_string gate)

let draw_input cr ~x ~y ~label ~value =
  Cairo.arc cr (x +. input_center.x) (y +. input_center.y) ~r:input_radius ~a1:0.
    ~a2:(2. *. Float.pi);
  set_color cr (color_of_value value);
  Cairo.fill_preserve cr;
  set_color cr ink;
  Cairo.set_line_width cr 2.;
  Cairo.stroke cr;
  centered_text cr ~cx:(x +. input_center.x) ~cy:(y +. input_center.y) ~size:14.
    ~color:(1., 1., 1.) label
