open ORun

let parse input =
  let lexer = ORun.Lexer.init input in
  let parser = ORun.Parser.init lexer in
  let prg = ORun.Parser.parse parser in
  match prg with
  | Ok (Program program) -> Ok program
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
      Program { statements = [(ExpressionStmt
                                 (BinaryOp
                                    { left = (IntegerLit 1); operator = (Plus);
                                      right = (IntegerLit 2) }))]
       }
      |}]
;;

let%expect_test "parser:trivialexpr2" =
  let input = "1-2" in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|
      Program { statements = [(ExpressionStmt
                                 (BinaryOp
                                    { left = (IntegerLit 1); operator = (Minus);
                                      right = (IntegerLit 2) }))]
       }
      |}]
;;

let%expect_test "parser:trivialexpr3" =
  let input = "10*2-5" in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|
      Program { statements = [(ExpressionStmt
                                 (BinaryOp
                                    { left =
                                      (BinaryOp
                                         { left = (IntegerLit 10);
                                           operator = (Astherisk);
                                           right = (IntegerLit 2) });
                                      operator = (Minus); right = (IntegerLit 5) }))]
       }
      |}]
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
      Program { statements = [(ExpressionStmt
                                 (BinaryOp
                                    { left = (IntegerLit 1); operator = (Plus);
                                      right =
                                      (BinaryOp
                                         { left = (IntegerLit 2);
                                           operator = (Astherisk);
                                           right = (IntegerLit 3) })
                                      }))]
       }
      |}]
;;

let%expect_test "parser:grouped_expr" =
  let input = "(1 + 2) * 3;" in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|
      Program { statements = [(ExpressionStmt
                                 (BinaryOp
                                    { left =
                                      (BinaryOp
                                         { left = (IntegerLit 1);
                                           operator = (Plus);
                                           right = (IntegerLit 2) });
                                      operator = (Astherisk);
                                      right = (IntegerLit 3) }))]
       }
      |}]
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
      Program { statements = [(LetStmt
                                 { identifier = { identifier = "x" };
                                   expression = (IntegerLit 1) });
                              (LetStmt
                                 { identifier = { identifier = "y" };
                                   expression = (IntegerLit 2) })]
       }
      |}]
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
      Program { statements = [(LetStmt
                                 { identifier = { identifier = "add" };
                                   expression =
                                   (FunctionLit
                                      { parameters =
                                        [{ identifier = "x" }; { identifier = "y" }
                                          ];
                                        body =
                                        (BlockExpr
                                           { block_stmts =
                                             [(ExpressionStmt
                                                 (BinaryOp
                                                    { left =
                                                      (Identifier
                                                         { identifier = "x" });
                                                      operator = (Plus);
                                                      right =
                                                      (Identifier
                                                         { identifier = "y" })
                                                      }))
                                               ]
                                             })
                                        })
                                   })]
       }

      |}]
;;

let%expect_test "parser:function_calls" =
  let input = "let x = add(1, 2);" in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|
      Program { statements = [(LetStmt
                                 { identifier = { identifier = "x" };
                                   expression =
                                   (FunctionCall
                                      { fn = { identifier = "add" };
                                        arguments =
                                        [(IntegerLit 1); (IntegerLit 2)] })
                                   })]
       }
      |}]
;;

let%expect_test "parser:array_indexing" =
  let input = "let x = arr[1];" in
  (*NOTE  arr[1;] would also make this test pass - see parser.ml ...*)
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|     
      Program { statements = [(LetStmt
                                 { identifier = { identifier = "x" };
                                   expression =
                                   (ArrayIndex
                                      { name = { identifier = "arr" };
                                        index = (IntegerLit 1) })
                                   })]
       }
      |}]
;;

let%expect_test "parser:expr_stmts" =
  let input =
    "let main = fn() { if (y > 1) { return 0 } }"
  in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|
      Program { statements = [(LetStmt
                                 { identifier = { identifier = "main" };
                                   expression =
                                   (FunctionLit
                                      { parameters = [];
                                        body =
                                        (BlockExpr
                                           { block_stmts =
                                             [(ExpressionStmt
                                                 (IfCond
                                                    { condition =
                                                      (BinaryOp
                                                         { left =
                                                           (Identifier
                                                              { identifier = "y" });
                                                           operator = (GreaterThan);
                                                           right = (IntegerLit 1) });
                                                      consequence =
                                                      (BlockExpr
                                                         { block_stmts =
                                                           [(ReturnStmt
                                                               { expression =
                                                                 (IntegerLit 0) })
                                                             ]
                                                           });
                                                      alternative = None }))
                                               ]
                                             })
                                        })
                                   })]
       }
      |}]
;;

let%expect_test "parser:while_stmt" =
  let input =
    "let main = fn() { while (y > 1) { return -1 } }"
  in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|     
      Program { statements = [(LetStmt
                                 { identifier = { identifier = "main" };
                                   expression =
                                   (FunctionLit
                                      { parameters = [];
                                        body =
                                        (BlockExpr
                                           { block_stmts =
                                             [(ExpressionStmt
                                                 (WhileLoop
                                                    { condition =
                                                      (BinaryOp
                                                         { left =
                                                           (Identifier
                                                              { identifier = "y" });
                                                           operator = (GreaterThan);
                                                           right = (IntegerLit 1) });
                                                      body =
                                                      (BlockExpr
                                                         { block_stmts =
                                                           [(ReturnStmt
                                                               { expression =
                                                                 (UnaryOp
                                                                    { operator =
                                                                      (Minus);
                                                                      right =
                                                                      (IntegerLit 1)
                                                                      })
                                                                 })
                                                             ]
                                                           })
                                                      }))
                                               ]
                                             })
                                        })
                                   })]
       }
      |}]
;;
