let eval input =
  let lexer = ORun.Lexer.init input in
  let parser = ORun.Parser.init lexer in
  let ast_node = ORun.Parser.parse parser in
  match ast_node with
  | Ok (ORun.Ast.Program program) -> ORun.Eval.eval program
  | _ -> failwith "Failed to parse input"
;;

let repl () =
  print_endline "Welcome to the REPL!";
  print_endline "Type 'exit' to exit the REPL.";
  let rec loop () =
    print_string "> ";
    let input = read_line () in
    if input <> "exit"
       (* then (
          try
          let result = eval input in
          print_endline (ORun.Object.Value.to_string result)
          with
          | ORun.Eval.RuntimeError error -> print_endline error TODO: Implement RuntimeError in Eval
          | ORun.Parser.ParseError error -> print_endline error);
       *)

       (* NOTE: the current state of the interpreter solely works for integers :/
          This is just a preliminary version. The goal is for it to be easily extensible.
       *)
    then (
      match eval input with
      | Ok result ->
        print_endline (ORun.Object.Value.show result)
      | _ -> prerr_endline "Failed to parse input")
    else loop ()
  in
  loop ()
;;

let () = repl ()
