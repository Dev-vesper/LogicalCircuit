type node_id = int

type kind = Input | Gate of Gate.t

type error =
  | Unknown_node of node_id
  | Not_a_gate of node_id
  | Not_an_input of node_id
  | Arity_exceeded of node_id
  | Incomplete of node_id
  | Cyclic of node_id list

exception Invalid of error

let close_cycle = function [] -> [] | first :: _ as cycle -> cycle @ [ first ]

let error_to_string = function
  | Unknown_node id -> Printf.sprintf "unknown node %d" id
  | Not_a_gate id -> Printf.sprintf "node %d is an input node and accepts no wire" id
  | Not_an_input id -> Printf.sprintf "node %d is a gate, not an input" id
  | Arity_exceeded id -> Printf.sprintf "node %d has every input port wired already" id
  | Incomplete id -> Printf.sprintf "node %d has unwired input ports" id
  | Cyclic ids ->
      Printf.sprintf "feedback cycle: %s"
        (String.concat " -> " (List.map string_of_int (close_cycle ids)))

type node = {
  kind : kind;
  mutable input : bool;  (** only meaningful when [kind = Input] *)
  mutable wires : node_id list;  (** incoming, in port order *)
  mutable successors : node_id list;  (** outgoing, fan-out *)
}

type t = { mutable nodes : node Dynarray.t }

let create () = { nodes = Dynarray.create () }
let node_count t = Dynarray.length t.nodes

let find t id =
  if id < 0 || id >= Dynarray.length t.nodes then Error (Unknown_node id)
  else Ok (Dynarray.get t.nodes id)

let get t id = match find t id with Ok node -> node | Error e -> raise (Invalid e)
let kind t id = match find t id with Ok node -> Ok node.kind | Error e -> Error e

let push t kind input =
  let id = Dynarray.length t.nodes in
  Dynarray.add_last t.nodes { kind; input; wires = []; successors = [] };
  id

let add_input t value = push t Input value
let add_gate t gate = push t (Gate gate) false

let input_nodes t =
  let count = Dynarray.length t.nodes in
  let rec go id acc =
    if id >= count then List.rev acc
    else
      let acc =
        match (Dynarray.get t.nodes id).kind with Input -> id :: acc | Gate _ -> acc
      in
      go (id + 1) acc
  in
  go 0 []

let wires t id = (get t id).wires
let successors t id = (get t id).successors

let iter_wires t f =
  let count = Dynarray.length t.nodes in
  for src = 0 to count - 1 do
    List.iter (fun dst -> f src dst) (Dynarray.get t.nodes src).successors
  done

let connect t ~src ~dst =
  match find t src with
  | Error e -> Error e
  | Ok src_node -> (
      match find t dst with
      | Error e -> Error e
      | Ok dst_node -> (
          match dst_node.kind with
          | Input -> Error (Not_a_gate dst)
          | Gate g ->
              if List.length dst_node.wires >= Gate.arity g then Error (Arity_exceeded dst)
              else begin
                dst_node.wires <- dst_node.wires @ [ src ];
                src_node.successors <- dst :: src_node.successors;
                Ok ()
              end))

let connect_exn t ~src ~dst =
  match connect t ~src ~dst with Ok () -> () | Error e -> raise (Invalid e)

let set_input t id value =
  match find t id with
  | Error e -> Error e
  | Ok node -> (
      match node.kind with
      | Input ->
          node.input <- value;
          Ok ()
      | Gate _ -> Error (Not_an_input id))

let get_input t id =
  match find t id with Ok { kind = Input; input; _ } -> Some input | _ -> None

type color = White | Grey | Black

let simulate t =
  let count = Dynarray.length t.nodes in
  let values = Array.make count false in
  let colors = Array.make count White in
  let path = ref [] in
  let cycle_from id =
    let rec take acc = function
      | [] -> List.rev acc
      | x :: rest -> if x = id then List.rev (x :: acc) else take (x :: acc) rest
    in
    take [] !path
  in
  let rec visit id =
    match colors.(id) with
    | Black -> Ok ()
    | Grey -> Error (Cyclic (cycle_from id))
    | White -> (
        let node = Dynarray.get t.nodes id in
        colors.(id) <- Grey;
        path := id :: !path;
        match visit_wires node.wires with
        | Error e -> Error e
        | Ok () ->
            let wired =
              match node.kind with
              | Input -> true
              | Gate g -> List.length node.wires = Gate.arity g
            in
            if not wired then Error (Incomplete id)
            else begin
              values.(id) <-
                (match node.kind with
                | Input -> node.input
                | Gate g -> Gate.eval g (List.map (Array.get values) node.wires));
              colors.(id) <- Black;
              path := List.tl !path;
              Ok ()
            end)
  and visit_wires = function
    | [] -> Ok ()
    | w :: rest -> ( match visit w with Ok () -> visit_wires rest | Error e -> Error e)
  in
  let rec loop id =
    if id >= count then Ok values
    else match visit id with Ok () -> loop (id + 1) | Error e -> Error e
  in
  loop 0

let validate t = Result.map ignore (simulate t)

let eval t id =
  match find t id with
  | Error e -> raise (Invalid e)
  | Ok _ -> ( match simulate t with Ok values -> values.(id) | Error e -> raise (Invalid e))
