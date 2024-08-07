open Ast
open Token
open Object
open Env

let ( let* ) res f = Base.Result.bind res ~f

type t =
  { mutable env : Environment.t
  ; traverser : Ast.traverser
  }

let init_eval program =
  { env = Environment.create ()
  ; traverser = Ast.init_traverser program
  }
;;

let get_key t key = Environment.get t.env key

let rec eval_stmt stmt t =
  match stmt with
  | Some (Ast.ExpressionStmt expression) ->
    eval_expr expression
  | Some (Ast.LetStmt _) -> eval_let_stmt stmt t
  | _ -> Base.Error "Not implemented"

and eval_let_stmt stmt t =
  match stmt with
  | Some (Ast.LetStmt { identifier; expression }) ->
    let* value = eval_expr expression in
    Environment.add
      t.env
      (identifier_to_string identifier)
      value;
    Base.Ok Object.Value.Null
  | _ ->
    Base.Ok (Object.Value.Error "Invalid let statement")

and eval_expr expr =
  let res =
    match expr with
    | IntegerLit i -> Base.Ok (Value.Number i)
    | BooleanLit i -> Base.Ok (Value.Boolean i)
    | NullLit -> Base.Ok Value.Null
    | UnaryOp op -> eval_prefix_expr op.operator op.right
    | BinaryOp op ->
      eval_infix_expr op.operator op.left op.right
    | _ -> Base.Error "Invalid expression type"
  in
  match res with
  | Base.Ok v -> Base.Ok v
  | Base.Error _ ->
    Base.Ok
      (Object.Value.Error
         ("Invalid expression " ^ Ast.show_expression expr))
(* NOTE: For now, printing the ast of the expression should be fine -> it would be better to provide insights in
   a more compactful way*)

and eval_prefix_expr operator expr =
  match operator with
  | Bang -> eval_bang_expr expr
  | Minus -> eval_minus_expr expr
  | _ -> Base.Error "Invalid prefix operator"

and eval_infix_expr operator left right =
  let* left_val = eval_expr left in
  let* right_val = eval_expr right in
  match operator with
  | Plus ->
    (match left_val, right_val with
     | Object.Value.Number l, Object.Value.Number r ->
       Base.Ok (Object.Value.Number (l + r))
     | Object.Value.String l, Object.Value.String r ->
       Base.Ok (Object.Value.String (l ^ r))
     | _ -> Base.Error "Invalid infix operation")
  | Minus ->
    (match left_val, right_val with
     | Object.Value.Number l, Object.Value.Number r ->
       Base.Ok (Object.Value.Number (l - r))
     | _ -> Base.Error "Invalid infix operation")
  | Astherisk ->
    (match left_val, right_val with
     | Object.Value.Number l, Object.Value.Number r ->
       Base.Ok (Object.Value.Number (l * r))
     | _ -> Base.Error "Invalid infix operation")
  | Slash ->
    (match left_val, right_val with
     | Object.Value.Number l, Object.Value.Number r ->
       Base.Ok (Object.Value.Number (l / r))
     | _ -> Base.Error "Invalid infix operation")
  | Equal ->
    (match left_val, right_val with
     | Object.Value.Number l, Object.Value.Number r ->
       Base.Ok (Object.Value.Boolean (l = r))
     | Object.Value.Boolean l, Object.Value.Boolean r ->
       Base.Ok (Object.Value.Boolean (l = r))
     | Object.Value.String l, Object.Value.String r ->
       Base.Ok (Object.Value.Boolean (l = r))
     | _ -> Base.Error "Invalid infix operation")
  | NotEqual ->
    (match left_val, right_val with
     | Object.Value.Number l, Object.Value.Number r ->
       Base.Ok (Object.Value.Boolean (l <> r))
     | Object.Value.Boolean l, Object.Value.Boolean r ->
       Base.Ok (Object.Value.Boolean (l <> r))
     | Object.Value.String l, Object.Value.String r ->
       Base.Ok (Object.Value.Boolean (l <> r))
     | _ -> Base.Error "Invalid infix operation")
  | LessThan ->
    (match left_val, right_val with
     | Object.Value.Number l, Object.Value.Number r ->
       Base.Ok (Object.Value.Boolean (l < r))
     | _ -> Base.Error "Invalid infix operation")
  | GreaterThan ->
    (match left_val, right_val with
     | Object.Value.Number l, Object.Value.Number r ->
       Base.Ok (Object.Value.Boolean (l > r))
     | _ -> Base.Error "Invalid infix operation")
  | _ -> Base.Error "Invalid infix operation"

and eval_bang_expr right =
  let* value = eval_expr right in
  match value with
  | Object.Value.Boolean b ->
    Base.Ok (Object.Value.Boolean (not b))
  | Null -> Base.Ok (Object.Value.Boolean true)
  | _ -> Base.Error "Not implemented"

and eval_minus_expr right =
  let* value = eval_expr right in
  match value with
  | Object.Value.Number n ->
    Base.Ok (Object.Value.Number (-n))
  | _ -> Base.Error "Not implemented"

and eval evaluator =
  (* eval goes throught the AST using the traverser iterator *)
  (* It returns the result of the last statement, for REPL purposes *)
  let traverser =
    evaluator.traverser |> Ast.advance_traverser
  in
  let rec eval_stmts' traverser ret =
    (* pp_env evaluator.env; *)
    match traverser with
    | Some t ->
      let* curr = eval_stmt t.curr_stmt evaluator in
      eval_stmts' (Ast.advance_traverser t) curr
    | None -> Base.Ok ret
  in
  eval_stmts' traverser Object.Value.Null

and pp_env env =
  Environment.to_string env |> Printf.printf "%s\n"
;;
