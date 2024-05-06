
type t = 
| Illegal
(* Identifiers *) 
| Identifier of string
| IntegerLit of string
| StringLit of string
(* Operators*)
| Assign
| Plus
| Minus
| Astherisk
| Slash
| Equal
| NotEqual
| LessThan
| GreaterThan
| And
| Or
| Bang
| EOF
(* Delimiters *)
| Comma
| Semicolon
| LeftParen
| RightParen
| LeftBrace
| RightBrace
| Quote
(* Keywords *)
| If
| Else
| While
| For
| Return
| Int
| Bool
| True
| False
| Break
| Continue
| Function
| Let
[@@deriving show { with_path = false }]

let (==) (token1 : t) (token2 : t) : bool =
  match (token1, token2) with
  | (Illegal, Illegal) -> true
  | (Identifier id1, Identifier id2) -> id1 = id2
  | (IntegerLit lit1, IntegerLit lit2) -> lit1 = lit2
  | (StringLit lit1, StringLit lit2) -> lit1 = lit2
  | (Assign, Assign) -> true
  | (Plus, Plus) -> true
  | (Minus, Minus) -> true
  | (Astherisk, Astherisk) -> true
  | (Slash, Slash) -> true
  | (Equal, Equal) -> true
  | (NotEqual, NotEqual) -> true
  | (LessThan, LessThan) -> true
  | (GreaterThan, GreaterThan) -> true
  | (And, And) -> true
  | (Or, Or) -> true
  | (Bang, Bang) -> true
  | (EOF, EOF) -> true
  | (Comma, Comma) -> true
  | (Semicolon, Semicolon) -> true
  | (LeftParen, LeftParen) -> true
  | (RightParen, RightParen) -> true
  | (LeftBrace, LeftBrace) -> true
  | (RightBrace, RightBrace) -> true
  | (Quote, Quote) -> true
  | (If, If) -> true
  | (Else, Else) -> true
  | (While, While) -> true
  | (For, For) -> true
  | (Return, Return) -> true
  | (Int, Int) -> true
  | (Bool, Bool) -> true
  | (True, True) -> true
  | (False, False) -> true
  | (Break, Break) -> true
  | (Continue, Continue) -> true
  | (Function, Function) -> true
  | (Let, Let) -> true
  | _ -> false

(** TODO: optionally add the following tokens:
  - underscores
*)

(* TODO: handle strings at tokenization *)
