open ORun

let parse input =
  let lexer = ORun.Lexer.init input in
  let parser = ORun.Parser.init lexer in
  let prg = ORun.Parser.parse parser in
  match prg with
  | Ok (Ast.Program program) -> Ok program
  | _ -> Error "unable to parse"
;;

let%expect_test "parser:trivialexpr" =
  let input = "1 + 2;" in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|     
    Program { statements = [(Ast.ExpressionStmt
                               (Ast.BinaryOp
                                  { Ast.left = (Ast.IntegerLit 1);
                                    operator = (Plus); right = (Ast.IntegerLit 2)
                                    }))]
     } |}]
;;

let%expect_test "parser:composed_expr" =
  let input = "1 + 2 * 3;" in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|     
    Program { statements = [(Ast.ExpressionStmt
                               (Ast.BinaryOp
                                  { Ast.left = (Ast.IntegerLit 1);
                                    operator = (Plus);
                                    right = 
                                    (Ast.BinaryOp
                                       { Ast.left = (Ast.IntegerLit 2);
                                         operator = (Astherisk);
                                         right = (Ast.IntegerLit 3) })
                                    }))]
     } |}]
;;

let%expect_test "parser:variables" =
  let input = "let x = 1; let y = 2;" in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|     
    Program { statements = [(Ast.LetStmt 
                               { Ast.identifier = { Ast.identifier = "x" }; 
                                 expression = (Ast.IntegerLit 1) });
                            (Ast.LetStmt
                               { Ast.identifier = { Ast.identifier = "y" };
                                 expression = (Ast.IntegerLit 2) })]
     } |}]
;;

let%expect_test "parser:functions" =
  let input = "let add = fn(x, y) { x + y; }" in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|     
    Program { statements = [(Ast.LetStmt
                               { Ast.identifier = { Ast.identifier = "add" };
                                 expression = 
                                 (Ast.FunctionLit
                                    { Ast.parameters = 
                                      [{ Ast.identifier = "x" };
                                        { Ast.identifier = "y" }];
                                      body =
                                      { Ast.block_stmts =
                                        [(Ast.ExpressionStmt
                                            (Ast.BinaryOp
                                               { Ast.left =
                                                 (Ast.Identifier
                                                    { Ast.identifier = "x" });
                                                 operator = (Plus);
                                                 right =
                                                 (Ast.Identifier
                                                    { Ast.identifier = "y" })
                                                 }))
                                          ]
                                        }
                                      })
                                 })]
     } 
      |}]
;;

(* TODO: function calls; array indexing in expressions; parenthesis in expressions *)
