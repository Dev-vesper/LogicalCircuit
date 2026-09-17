open Logic

type id = int

type node = {
  mutable id : id;
  kind : Circuit.kind;
  mutable x : float;
  mutable y : float;
  mutable level : bool;
  mutable ports : id option array;
  mutable routes : (float * float) list array;
}

type wire = { src : id; dst : id; port : int }

type t = { mutable nodes : node Dynarray.t }

let create () = { nodes = Dynarray.create () }
let count t = Dynarray.length t.nodes
let node t id = Dynarray.get t.nodes id
let iter t f = Dynarray.iter f t.nodes

let wires t =
  let found = ref [] in
  Dynarray.iter
    (fun n ->
      Array.iteri
        (fun port -> function
          | Some src -> found := { src; dst = n.id; port } :: !found
          | None -> ())
        n.ports)
    t.nodes;
  List.rev !found

let arity = function Circuit.Input -> 0 | Circuit.Gate g -> Gate.arity g

let add t ~kind ~x ~y =
  let id = Dynarray.length t.nodes in
  Dynarray.add_last t.nodes
    {
      id;
      kind;
      x;
      y;
      level = false;
      ports = Array.make (arity kind) None;
      routes = Array.make (arity kind) [];
    };
  id

let move t id ~x ~y =
  let n = node t id in
  n.x <- x;
  n.y <- y

let toggle t id = (node t id).level <- not (node t id).level

let free_port t dst =
  let ports = (node t dst).ports in
  let rec go i = if i >= Array.length ports then None else
    match ports.(i) with None -> Some i | Some _ -> go (i + 1)
  in
  go 0

let connect ?port t ~src ~dst =
  let n = node t dst in
  let free i = i >= 0 && i < Array.length n.ports && n.ports.(i) = None in
  match (match port with Some p when free p -> Some p | _ -> free_port t dst) with
  | None -> None
  | Some p ->
      n.ports.(p) <- Some src;
      Some p

let disconnect t ~dst ~port =
  let n = node t dst in
  n.ports.(port) <- None;
  n.routes.(port) <- []

let remove t id =
  let last = Dynarray.length t.nodes - 1 in
  Dynarray.iter
    (fun n ->
      if n.id > id then begin
        Array.iteri
          (fun port -> function
            | Some src when src = id ->
                n.ports.(port) <- None;
                n.routes.(port) <- []
            | Some src when src > id -> n.ports.(port) <- Some (src - 1)
            | _ -> ())
          n.ports;
        n.id <- n.id - 1
      end)
    t.nodes;
  for i = id to last - 1 do
    Dynarray.set t.nodes i (Dynarray.get t.nodes (i + 1))
  done;
  Dynarray.truncate t.nodes last

let clear t = Dynarray.clear t.nodes

let route t ~dst ~port =
  let n = node t dst in
  match n.ports.(port) with
  | None -> []
  | Some src ->
      let s = node t src in
      List.map (fun (ox, oy) -> (s.x +. ox, s.y +. oy)) n.routes.(port)

let set_route t ~dst ~port bends =
  let n = node t dst in
  match n.ports.(port) with
  | None -> ()
  | Some src ->
      let s = node t src in
      n.routes.(port) <- List.map (fun (x, y) -> (x -. s.x, y -. s.y)) bends

let build t =
  let c = Circuit.create () in
  Dynarray.iter
    (fun n ->
      ignore
        (match n.kind with
        | Circuit.Input -> Circuit.add_input c n.level
        | Circuit.Gate g -> Circuit.add_gate c g))
    t.nodes;
  Dynarray.iter
    (fun n ->
      Array.iter
        (function
          | Some src -> Circuit.connect_exn c ~src ~dst:n.id
          | None -> ())
        n.ports)
    t.nodes;
  c

let evaluate t =
  let c = build t in
  let values, cycle = Circuit.simulate_partial c in
  (c, values, cycle)
