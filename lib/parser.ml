open Stdlib

type t =
  { lexer : Lexer.t
  ; current_token : Token.t Base.option
  ; peek_token : Token.t Base.option
  }

let opt_to_res opt err =
  match opt with
  | Some x -> Base.Ok x
  | None -> Base.Error err
;;

type precedence =
  | LOWEST
  | EQUALS
  | LESSGREATER
  | SUM
  | PRODUCT
  | PREFIX
  | CALL
  | EOF
[@@deriving show, ord]

let ( <. ) p1 p2 = Stdlib.compare p1 p2 < 0

(* Overloads the monadic bind operator to be used with Result instead of option *)
let ( let* ) res f = Base.Result.bind res ~f

(* init needs to read two tokens - must initialize current_token and peek_token
   Lexer.next token returns a new lexer and the current token
*)

let init lexer =
  let lexer =
    { lexer; current_token = None; peek_token = None }
  in
  let next_lexer, curr_tok = Lexer.next_token lexer.lexer in
  let next_lexer, next_tok = Lexer.next_token next_lexer in
  { lexer = next_lexer
  ; current_token = curr_tok
  ; peek_token = next_tok
  }
;;

let advance parser =
  let next_lexer, next_peek =
    Lexer.next_token parser.lexer
  in
  { lexer = next_lexer
  ; current_token = parser.peek_token
  ; peek_token = next_peek
  }
;;

(* parse is the main method - parses all statements into a list *)

let expect_token parser token =
  match parser.current_token with
  | Some tok when Token.( == ) token tok ->
    Base.Ok (advance parser)
  | Some _ ->
    let* curr_token =
      opt_to_res parser.current_token "no current token"
    in
    Base.Error
      ("unexpected token: "
       ^ Token.show curr_token
       ^ " expected: "
       ^ Token.show token)
  | None -> Base.Error "no current token"
;;

let cur_precedence parser =
  match parser.current_token with
  | Some Token.Equal -> EQUALS
  | Some Token.NotEqual -> EQUALS
  | Some Token.LessThan -> LESSGREATER
  | Some Token.GreaterThan -> LESSGREATER
  | Some Token.Plus -> SUM
  | Some Token.Minus -> SUM
  | Some Token.Slash -> PRODUCT
  | Some Token.Astherisk -> PRODUCT
  | _ -> LOWEST
;;

let peek_precedence parser =
  match parser.peek_token with
  | Some Token.Equal -> EQUALS
  | Some Token.NotEqual -> EQUALS
  | Some Token.LessThan -> LESSGREATER
  | Some Token.GreaterThan -> LESSGREATER
  | Some Token.Plus -> SUM
  | Some Token.Minus -> SUM
  | Some Token.Slash -> PRODUCT
  | Some Token.Astherisk -> PRODUCT
  | _ -> LOWEST
;;

let rec parse_let_stmt parser =
  let* identifier, parser = parse_identifier parser in
  let* parser = expect_token parser Token.Assign in
  let* expression, parser =
    parse_expression parser LOWEST
  in
  Ok (Ast.LetStmt { identifier; expression }, parser)

and parse_return_stmt parser =
  let* parser = expect_token parser Token.Return in
  let* expression, parser =
    parse_expression parser LOWEST
  in
  Ok (Ast.ReturnStmt { expression }, parser)

and parse_expr_stmt parser =
  let* expression, parser =
    parse_expression parser LOWEST
  in
  Ok (Ast.ExpressionStmt expression, parser)

(* Returns a statement and the new parser state. On error, return the error message *)
and parse_statement parser =
  match parser.current_token with
  | Some Token.Let -> parse_let_stmt (advance parser)
  | Some Token.Return -> parse_return_stmt parser
  | _ -> parse_expr_stmt parser

(* parse_identifier returns an expression *)
and parse_identifier parser =
  match parser.current_token with
  | Some (Token.Identifier id) ->
    Base.Ok (Ast.new_identifier id, advance parser)
  | _ -> Base.Error "expected identifier"

and parse_block parser =
  let* parser = expect_token parser Token.LeftBrace in
  let rec parse_block' parser acc =
    match parser.current_token with
    | Some Token.RightBrace ->
      let stmts = List.rev acc in
      Base.Ok (advance parser, stmts)
    | _ ->
      (match parse_statement parser with
       | Base.Ok (stmt, parser) ->
         parse_block' parser (stmt :: acc)
       | Base.Error err -> Base.Error err)
  in
  let* parser, block = parse_block' parser [] in
  Base.Ok (parser, Ast.BlockExpr { block_stmts = block })

and parse_function parser =
  let* parser = expect_token parser Token.LeftParen in
  let rec parse_params parser acc =
    match parser.current_token with
    | Some Token.RightParen ->
      Base.Ok (List.rev acc, advance parser)
    | Some Token.Comma -> parse_params (advance parser) acc
    | Some (Token.Identifier id) ->
      parse_params
        (advance parser)
        (Ast.new_identifier id :: acc)
    | _ -> Base.Error "expected identifier"
  in
  let* params, parser = parse_params parser [] in
  let* parser, block = parse_block parser in
  Base.Ok
    ( Ast.FunctionLit (Ast.new_function_lit params block)
    , parser )

and parse_function_call parser =
  let* name, parser = parse_identifier parser in
  let* parser = expect_token parser Token.LeftParen in
  let rec parse_args parser acc =
    match parser.current_token with
    | Some Token.RightParen ->
      Base.Ok (List.rev acc, advance parser)
    | Some Token.Comma -> parse_args (advance parser) acc
    | _ ->
      (match parse_expression parser LOWEST with
       | Base.Ok (expr, parser) ->
         parse_args parser (expr :: acc)
       | Base.Error err -> Base.Error err)
  in
  let* args, parser = parse_args parser [] in
  Base.Ok
    ( Ast.FunctionCall (Ast.new_function_call name args)
    , parser )

and parse_array_index parser =
  let* name, parser = parse_identifier parser in
  let* parser =
    expect_token parser Token.LeftSquareBracket
  in
  let* index, parser = parse_expression parser LOWEST in
  (*NOTE: parse_expression is designed to chop off a possible semicolon
    this makes array indexing operations like arr[i-1;] possible
    Not allowing this semicolon would require an implementation of parse_expression for this operations.
    Let's just not bloat the parser for now :) *)
  let* parser =
    expect_token parser Token.RightSquareBracket
  in
  Base.Ok
    (Ast.ArrayIndex (Ast.new_array_index name index), parser)

and peek_after_identifier parser id =
  match parser.peek_token with
  | Some Token.LeftParen -> parse_function_call parser
  | Some Token.LeftSquareBracket -> parse_array_index parser
  | _ ->
    Base.Ok
      (Ast.Identifier { identifier = id }, advance parser)

and parse_grouped_expression parser =
  let* expression, parser =
    parse_expression parser LOWEST
  in
  let* parser = expect_token parser Token.RightParen in
  Base.Ok (expression, parser)

and parse_expression parser precedence =
  let* left, parser = parse_prefix_expr parser in
  (* We can derail from the interpreter book here -> parse_prefix_expr already advances the parser, so we can just check the current token *)
  match parser.current_token with
  | Some Token.Semicolon ->
    Base.Ok (left, advance parser)
    (* advance to chomp semicolon *)
  | _ ->
    if precedence <. cur_precedence parser
    then parse_infix_expr parser left
    else Base.Ok (left, parser)

and parse_if_expr parser =
  let* parser = expect_token parser Token.LeftParen in
  let* condition, parser = parse_expression parser LOWEST in
  let* parser = expect_token parser Token.RightParen in
  let* parser, consequence = parse_block parser in
  let* alternative, parser =
    match parser.current_token with
    | Some Token.Else ->
      let* parser, alternative =
        parse_block (advance parser)
      in
      Base.Ok (Some alternative, parser)
    | _ -> Base.Ok (None, parser)
  in
  Base.Ok
    ( Ast.IfCond { condition; consequence; alternative }
    , parser )

and parse_prefix_expr parser =
  match parser.current_token with
  | Some (Identifier id) -> peek_after_identifier parser id
  | Some (Token.IntegerLit i) ->
    Base.Ok
      (Ast.IntegerLit (Base.Int.of_string i), advance parser)
  | Some Token.Function -> parse_function (advance parser)
  | Some Token.LeftParen ->
    parse_grouped_expression (advance parser)
  | Some Token.If -> parse_if_expr (advance parser)
  | Some Token.Bang | Some Token.Minus ->
    let* right, parser =
      parse_expression (advance parser) PREFIX
    in
    let* operator =
      opt_to_res parser.current_token "expected operator"
    in
    Base.Ok (Ast.UnaryOp { operator; right }, advance parser)
  | Some t ->
    Base.Error
      (Format.sprintf "unexpected token: %s" (Token.show t))
  | None -> Base.Error "no current token"

and parse_infix_expr parser left =
  let* right, parser' =
    parse_expression
      (advance parser)
      (cur_precedence parser)
  in
  let* operator =
    opt_to_res parser.current_token "expected operator"
  in
  Base.Ok (Ast.BinaryOp { left; operator; right }, parser')
;;

let parse parser =
  let rec parse' parser acc =
    match parser.current_token with
    | None | Some Token.Illegal ->
      Base.Error "illegal token"
    (* Illegal tokens are not allowed. None tokens will not exist but we still need to pattern match exhaustively *)
    (* The lexer should always return Some EOF when no more tokens are there to parse *)
    | Some Token.EOF ->
      Base.Ok (Ast.Program { statements = List.rev acc })
    | Some _ ->
      (match parse_statement parser with
       | Base.Ok (stmt, parser) ->
         parse' parser (stmt :: acc)
       | Base.Error err -> failwith err)
  in
  parse' parser []
;;

(*
   TODO: create unit tests for each parser production (WIP)
   TODO: test return statements
   TODO: if, while statements
   TODO: negative numbers
   TODO: create custom parse_error type to provide diagnostics regarding possible errors (e.g., token,)
*)
