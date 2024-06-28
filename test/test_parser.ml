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

let%expect_test "parser:trivialexpr2" =
  let input = "1-2" in
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
                                    operator = (Minus);
                                    right = (Ast.IntegerLit 2) }))]
     } |}]
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
    Program { statements = [(Ast.ExpressionStmt
                               (Ast.BinaryOp
                                  { Ast.left = 
                                    (Ast.BinaryOp
                                       { Ast.left = (Ast.IntegerLit 10);
                                         operator = (Astherisk);
                                         right = (Ast.IntegerLit 2) });
                                    operator = (Minus);
                                    right = (Ast.IntegerLit 5) }))]
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

let%expect_test "parser:grouped_expr" =
  let input = "(1 + 2) * 3;" in
  let program = parse input in
  match program with
  | Error e -> print_endline e
  | Ok prg ->
    Ast.pp_program Format.std_formatter prg;
    [%expect
      {|     
    Program { statements = [(Ast.ExpressionStmt
                               (Ast.BinaryOp
                                  { Ast.left = 
                                    (Ast.BinaryOp
                                       { Ast.left = (Ast.IntegerLit 1);
                                         operator = (Plus);
                                         right = (Ast.IntegerLit 2) });
                                    operator = (Astherisk);
                                    right = (Ast.IntegerLit 3) }))]
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
                                      (Ast.BlockExpr
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
    Program { statements = [(Ast.LetStmt
                               { Ast.identifier = { Ast.identifier = "x" };
                                 expression = 
                                 (Ast.FunctionCall
                                    { Ast.fn = { Ast.identifier = "add" };
                                      arguments = 
                                      [(Ast.IntegerLit 1); (Ast.IntegerLit 2)] })
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
    Program { statements = [(Ast.LetStmt
                               { Ast.identifier = { Ast.identifier = "x" };
                                 expression = 
                                 (Ast.ArrayIndex
                                    { Ast.name = { Ast.identifier = "arr" };
                                      index = (Ast.IntegerLit 1) })
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
    Program { statements = [(Ast.LetStmt
                               { Ast.identifier = { Ast.identifier = "main" };
                                 expression =
                                 (Ast.FunctionLit
                                    { Ast.parameters = [];
                                      body =
                                      (Ast.BlockExpr
                                         { Ast.block_stmts =
                                           [(Ast.ExpressionStmt
                                               (Ast.IfCond
                                                  { Ast.condition =
                                                    (Ast.BinaryOp
                                                       { Ast.left =
                                                         (Ast.Identifier
                                                            { Ast.identifier =
                                                              "y" });
                                                         operator = (GreaterThan);
                                                         right =
                                                         (Ast.IntegerLit 1) });
                                                    consequence =
                                                    (Ast.BlockExpr
                                                       { Ast.block_stmts =
                                                         [(Ast.ReturnStmt
                                                             { Ast.expression =
                                                               (Ast.IntegerLit 0)
                                                               })
                                                           ]
                                                         });
                                                    alternative = None }))
                                             ]
                                           })
                                      })
                                 })]
     }|}]
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
    Program { statements = [(Ast.LetStmt
                               { Ast.identifier = { Ast.identifier = "main" };
                                 expression =
                                 (Ast.FunctionLit
                                    { Ast.parameters = [];
                                      body =
                                      (Ast.BlockExpr
                                         { Ast.block_stmts =
                                           [(Ast.ExpressionStmt
                                               (Ast.WhileLoop
                                                  { Ast.condition =
                                                    (Ast.BinaryOp
                                                       { Ast.left =
                                                         (Ast.Identifier
                                                            { Ast.identifier =
                                                              "y" });
                                                         operator = (GreaterThan);
                                                         right =
                                                         (Ast.IntegerLit 1) });
                                                    body =
                                                    (Ast.BlockExpr
                                                       { Ast.block_stmts =
                                                         [(Ast.ReturnStmt
                                                             { Ast.expression =
                                                               (Ast.UnaryOp
                                                                  { Ast.operator =
                                                                    (Minus);
                                                                    right =
                                                                    (Ast.IntegerLit
                                                                       1)
                                                                    })
                                                               })
                                                           ]
                                                         })
                                                    }))
                                             ]
                                           })
                                      })
                                 })]
     }|}]
;;
