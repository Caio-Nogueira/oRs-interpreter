open Base
open OUnit2
(*TODO: eval doesn't necessarily need to be in every single test -> evaluating expressions is inherently stateless *)

let eval_trivial_expr _ =
  let program =
    ORun.Ast.new_program
      [ ORun.Ast.ExpressionStmt (ORun.Ast.IntegerLit 1) ]
  in
  let expected_result =
    Base.Result.Ok (ORun.Object.Value.Number 1)
  in
  let actual_result = ORun.Eval.eval program in
  [%test_eq: (ORun.Object.Value.t, string) Base.Result.t]
    actual_result
    expected_result
;;

let eval_trivial_expr2 _ =
  let program =
    ORun.Ast.new_program
      [ ORun.Ast.ExpressionStmt
          (ORun.Ast.BinaryOp
             { left = ORun.Ast.IntegerLit 2
             ; operator = ORun.Token.Astherisk
             ; right = ORun.Ast.IntegerLit 3
             })
      ]
  in
  let expected_result =
    Base.Result.Ok (ORun.Object.Value.Number 6)
  in
  let actual_result = ORun.Eval.eval program in
  [%test_eq: (ORun.Object.Value.t, string) Base.Result.t]
    actual_result
    expected_result
;;

let eval_trivial_expr3 _ =
  let program =
    ORun.Ast.new_program
      [ ORun.Ast.ExpressionStmt
          (ORun.Ast.BinaryOp
             { left = ORun.Ast.IntegerLit 2
             ; operator = ORun.Token.Astherisk
             ; right =
                 ORun.Ast.BinaryOp
                   { left = ORun.Ast.IntegerLit 1
                   ; operator = ORun.Token.Plus
                   ; right = ORun.Ast.IntegerLit 2
                   }
             })
      ]
  in
  let expected_result =
    Base.Result.Ok (ORun.Object.Value.Number 6)
  in
  let actual_result = ORun.Eval.eval program in
  [%test_eq: (ORun.Object.Value.t, string) Base.Result.t]
    actual_result
    expected_result
;;

let suite =
  "Test ORun evaluation"
  >::: [ "eval_trivial_expr" >:: eval_trivial_expr
       ; "eval_trivial_expr2" >:: eval_trivial_expr2
       ; "eval_trivial_expr3" >:: eval_trivial_expr3
       ]
;;

let () = run_test_tt_main suite
