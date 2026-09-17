type row = { assignment : bool list; results : bool list }

type t = { inputs : Circuit.node_id list; outputs : Circuit.node_id list; rows : row list }

type error = Circuit_error of Circuit.error | Too_many_inputs of int

let max_inputs = 16

let error_to_string = function
  | Circuit_error e -> Circuit.error_to_string e
  | Too_many_inputs n ->
      Printf.sprintf "refusing to enumerate %d inputs (the maximum is %d)" n max_inputs

let default_outputs circuit =
  let count = Circuit.node_count circuit in
  let rec go id acc =
    if id >= count then List.rev acc
    else
      let is_sink =
        (match Circuit.kind circuit id with Ok (Circuit.Gate _) -> true | _ -> false)
        && Circuit.successors circuit id = []
      in
      go (id + 1) (if is_sink then id :: acc else acc)
  in
  go 0 []

let bit i k = (i lsr k) land 1 = 1

let of_circuit ?outputs circuit =
  let inputs = Circuit.input_nodes circuit in
  let count = List.length inputs in
  if count > max_inputs then Error (Too_many_inputs count)
  else
    let saved =
      List.map
        (fun id -> (id, Option.value ~default:false (Circuit.get_input circuit id)))
        inputs
    in
    let outputs =
      match outputs with Some given -> given | None -> default_outputs circuit
    in
    let assign i =
      let rec go k = function
        | [] -> Ok ()
        | id :: rest -> (
            match Circuit.set_input circuit id (bit i (count - 1 - k)) with
            | Ok () -> go (k + 1) rest
            | Error e -> Error (Circuit_error e))
      in
      go 0 inputs
    in
    let build i =
      match assign i with
      | Error e -> Error e
      | Ok () -> (
          match Circuit.simulate circuit with
          | Error e -> Error (Circuit_error e)
          | Ok values ->
              Ok
                {
                  assignment = List.init count (fun k -> bit i (count - 1 - k));
                  results = List.map (Array.get values) outputs;
                })
    in
    let rec go i acc =
      if i >= 1 lsl count then Ok (List.rev acc)
      else match build i with Error e -> Error e | Ok row -> go (i + 1) (row :: acc)
    in
    let result = Result.map (fun rows -> { inputs; outputs; rows }) (go 0 []) in
    List.iter
      (fun (id, value) -> ignore (Circuit.set_input circuit id value))
      saved;
    result

let name_of_index k =
  if k < 26 then String.make 1 (Char.chr (Char.code 'A' + k))
  else Printf.sprintf "in%d" k

let to_string t =
  let bit_string = function true -> "1" | false -> "0" in
  let inputs_header =
    String.concat " " (List.init (List.length t.inputs) name_of_index)
  in
  let output_names = List.map (fun id -> Printf.sprintf "out%d" id) t.outputs in
  let width =
    List.fold_left (fun w name -> max w (String.length name)) 1 output_names
  in
  let outputs_block cells =
    String.concat " " (List.map (Printf.sprintf "%-*s" width) cells)
  in
  let join left right =
    if left = "" then right else if right = "" then left else left ^ " | " ^ right
  in
  let line assignment results =
    join
      (String.concat " " (List.map bit_string assignment))
      (outputs_block (List.map bit_string results))
  in
  let header = join inputs_header (outputs_block output_names) in
  let ruler = String.map (function '|' -> '+' | ' ' -> ' ' | _ -> '-') header in
  String.concat "\n"
    (header :: ruler :: List.map (fun row -> line row.assignment row.results) t.rows)
