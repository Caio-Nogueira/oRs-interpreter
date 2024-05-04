
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


(** TODO: optionally add the following tokens:
  - underscores
*)

(* TODO: handle strings at tokenization *)
