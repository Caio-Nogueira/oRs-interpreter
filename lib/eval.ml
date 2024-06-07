let rec eval_stmt stmt =
  match stmt with
  | Some (Ast.ExpressionStmt expression) ->
    eval_expr expression
  | Some (Ast.LetStmt _) -> Base.Error "Not implemented"
  | _ -> Base.Error "Not implemented"

and eval_expr expr =
  match expr with
  | Ast.IntegerLit i -> Base.Ok (Object.Value.Number i)
  | _ -> Base.Error "Not implemented"

and eval program =
  let init = Ast.init_traverser program in
  let traverser = Ast.advance_traverser init in
  match traverser with
  | Some t -> eval_stmt t.curr_stmt
  | None -> Base.Ok Object.Value.Nil
;;
