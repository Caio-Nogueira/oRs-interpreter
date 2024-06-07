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
  | LeftSquareBracket
  | RightSquareBracket
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
  | Null
[@@deriving show { with_path = false }]

let ( == ) (token1 : t) (token2 : t) : bool =
  show token1 = show token2
;;

let pp fmt token = Format.fprintf fmt "(%s)" (show token)
