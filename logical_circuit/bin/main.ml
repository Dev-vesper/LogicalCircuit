open Logic

(* A three-input majority voter: out = (a and b) or (b and c) or (a and c).
   Every input fans out to two gates, and because Or only offers two ports the third
   term has to come in through a second Or. *)
let majority () =
  let circuit = Circuit.create () in
  let a = Circuit.add_input circuit false in
  let b = Circuit.add_input circuit false in
  let c = Circuit.add_input circuit false in
  let a_and_b = Circuit.add_gate circuit Gate.And in
  let b_and_c = Circuit.add_gate circuit Gate.And in
  let a_and_c = Circuit.add_gate circuit Gate.And in
  let first_two = Circuit.add_gate circuit Gate.Or in
  let out = Circuit.add_gate circuit Gate.Or in
  List.iter
    (fun (src, dst) -> Circuit.connect_exn circuit ~src ~dst)
    [
      (a, a_and_b);
      (b, a_and_b);
      (b, b_and_c);
      (c, b_and_c);
      (a, a_and_c);
      (c, a_and_c);
      (a_and_b, first_two);
      (b_and_c, first_two);
      (first_two, out);
      (a_and_c, out);
    ];
  (circuit, out)

let show_majority () =
  let circuit, out = majority () in
  print_endline "majority voter: out is 1 when at least two of A, B and C are 1\n";
  match Truth_table.of_circuit ~outputs:[ out ] circuit with
  | Error e -> Printf.printf "  failed: %s\n" (Truth_table.error_to_string e)
  | Ok table ->
      print_string (Truth_table.to_string table);
      print_newline ()

(* Every failure a UI has to cope with: two it can detect before drawing a wire, and
   two that only the simulation can see. *)
let show_rejections () =
  let circuit = Circuit.create () in
  let a = Circuit.add_input circuit false in
  let b = Circuit.add_input circuit false in
  let conjunction = Circuit.add_gate circuit Gate.And in
  Circuit.connect_exn circuit ~src:a ~dst:conjunction;
  Circuit.connect_exn circuit ~src:b ~dst:conjunction;
  Printf.printf "a third wire on an And:      %s\n"
    (match Circuit.connect circuit ~src:b ~dst:conjunction with
    | Ok () -> "unexpectedly accepted"
    | Error e -> Circuit.error_to_string e);
  Printf.printf "a wire aimed at an input:    %s\n"
    (match Circuit.connect circuit ~src:conjunction ~dst:a with
    | Ok () -> "unexpectedly accepted"
    | Error e -> Circuit.error_to_string e);
  let dangling = Circuit.create () in
  ignore (Circuit.add_gate dangling Gate.Not);
  Printf.printf "a Not gate with no wire:     %s\n"
    (match Circuit.simulate dangling with
    | Ok _ -> "unexpectedly simulated"
    | Error e -> Circuit.error_to_string e);
  let feedback = Circuit.create () in
  let inverts = Circuit.add_gate feedback Gate.Not in
  Circuit.connect_exn feedback ~src:inverts ~dst:inverts;
  Printf.printf "a Not gate wired to itself:  %s\n"
    (match Circuit.simulate feedback with
    | Ok _ -> "unexpectedly simulated"
    | Error e -> Circuit.error_to_string e)

let () =
  show_majority ();
  print_endline "rejections:";
  show_rejections ()
