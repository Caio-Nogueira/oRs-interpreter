open Stdlib;;

type t = {
  lexer: Lexer.t;
  current_token: Token.t Base.option;
  peek_token: Token.t Base.option;
}

let opt_to_res opt err = 
  match opt with
  | Some x -> Base.Ok x
  | None -> Base.Error err

type precedence = 
  | LOWEST
  | EQUALS
  | LESSGREATER
  | SUM
  | PRODUCT
  | PREFIX
  | CALL
[@@deriving show, ord]

let (<.) p1 p2 = Stdlib.compare p1 p2 < 0

(* Overloads the monadic bind operator to be used with Result instead of option *)
let (let*) res f = Base.Result.bind res ~f

(* init needs to read two tokens - must initialize current_token and peek_token
  Lexer.next token returns a new lexer and the current token 
*)

let init lexer = 
  let lexer = {lexer; current_token = None; peek_token = None} in
  let next_lexer, curr_tok = Lexer.next_token lexer.lexer in
  let next_lexer, next_tok = Lexer.next_token next_lexer in
  { lexer = next_lexer; current_token = curr_tok; peek_token = next_tok }


let advance parser =
  let next_lexer, next_peek = Lexer.next_token parser.lexer in
  { lexer = next_lexer; current_token = parser.peek_token; peek_token = next_peek }

(* parse is the main method - parses all statements into a list *)

let expect_token parser token = 
  match parser.current_token with
  | Some tok when Token.(==) token tok -> Base.Ok (advance parser)
  | Some _ -> Base.Error ("unexpected token: " ^ (Token.show token))
  | None -> Base.Error ("no current token")

(* parse_identifier returns an expression *)
let parse_identifier parser = 
  match parser.current_token with
  | Some Token.Identifier id -> Base.Ok (Ast.new_identifier id, advance parser)
  | _ -> Base.Error "expected identifier"

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

let rec parse_expression parser precedence =
  let* left, parser = parse_prefix_expr parser in
  (* We can derail from the book here -> parse_prefix_expr already advances the parser, so we can just check the current token *)
  match parser.current_token with
  | Some Token.Semicolon -> Base.Ok (left, advance parser)
  | _ -> if precedence <. peek_precedence parser then parse_infix_expr parser left
         else Base.Ok (left, parser)

and parse_prefix_expr parser = 
  match parser.current_token with
  | Some Identifier id -> Base.Ok (Ast.Identifier {identifier = id}, advance parser)
  | Some Token.IntegerLit i -> Base.Ok (Ast.IntegerLit (Base.Int.of_string i), advance parser)
  | Some Token.Bang
  | Some Token.Minus -> let* right, parser = parse_expression (advance parser) PREFIX in
                        let* operator = opt_to_res parser.current_token "expected operator" in
                        Base.Ok (Ast.UnaryOp {operator = operator; right}, advance parser)
  | _ -> Base.Error "expected prefix expression"

and parse_infix_expr parser left =
  let precedence = cur_precedence parser in
  let* right, parser = parse_expression (advance parser) precedence in
  let* operator = opt_to_res parser.current_token "expected operator" in
  Base.Ok (Ast.BinaryOp {left; operator; right}, parser)

let parse_let_stmt parser = 
  let* identifier, parser = parse_identifier parser in
  let* parser = expect_token parser Token.Assign in
  let* expression, parser = parse_expression parser LOWEST in
  Ok (Ast.LetStmt {identifier; expression}, parser)

let parse_return_stmt parser =
  let* parser = expect_token parser Token.Return in
  let* expression, parser = parse_expression parser LOWEST in
  Ok (Ast.ReturnStmt {expression}, parser)

let parse_expr_stmt parser = 
  let* expression, parser = parse_expression parser LOWEST in
  Ok (Ast.ExpressionStmt expression, parser)

(* Returns a statement and the new parser state. On error, return the error message *)
let parse_statement parser = 
    match parser.current_token with
    | Some Token.Let -> parse_let_stmt (advance parser) 
    | Some Token.Return -> parse_return_stmt parser
    | _ -> parse_expr_stmt parser 


let parse parser = 
  let rec parse' parser acc = 
    match parser.current_token with
    | None
    | Some Token.Illegal -> failwith "illegal token"
    (* Illegal tokens are not allowed. None tokens will not exist but we still need to pattern match exhaustively *)
    (* The lexer should always return Some EOF when no more tokens are there to parse *)
    | Some Token.EOF -> List.rev acc
    | Some _ -> 
      match (parse_statement parser) with
      | Base.Ok (stmt, parser) -> parse' parser (stmt :: acc)
      | Base.Error err -> failwith err 
  in
  parse' parser []
  
(*
TODO: create unit tests for each parser production
TODO: if, while statements 
TODO: function literals / function calls (expressions)
TODO: array indexing (expressions)
TODO: create custom parse_error type to provide insights regarding possible errors (e.g., token,) 
*)
  