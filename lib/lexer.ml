open Token;;

type t = {
  input : string;
  position: int;
  ch: char option;
}
[@@deriving show]

let init input = {
  input;
  position = 0;
  ch = if String.length input = 0 then None else Some input.[0];
}

(* TODO: besides white spaces, the language should also support comments*)
let skip_white_space lexer = 
  let rec skip_white_space' lexer = 
    match lexer.ch with
    | Some ch when ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r' -> 
      let position = lexer.position + 1 in
      let ch = if position >= String.length lexer.input then None else Some lexer.input.[position] in
      skip_white_space' { lexer with position; ch }
    | _ -> lexer
  in
  skip_white_space' lexer

(** shifts the lexer to the position immediately after*)
let next_position lexer = 
  let position = lexer.position + 1 in
  {
    input = lexer.input;
    position;
    ch = if lexer.position + 1 >= String.length lexer.input then None else Some lexer.input.[lexer.position + 1];
  }

let advance lexer = 
  lexer |> next_position |> skip_white_space
  
let peek lexer ch ~whenMatched ~default = 
  lexer |> next_position |> fun lexer ->
  match lexer.ch with
  | Some c when c = ch -> advance lexer, Some whenMatched
  | _ -> skip_white_space lexer, Some default (* Next char could be a white space -> need to advance the lexer on to the next char to evaluate*)

let is_alnum ch = 
    let code = Char.code ch in
  (code >= 65 && code <= 90) || (code >= 97 && code <= 122) || (code >= 48 && code <= 57) (* A-Z, a-z, 0-9 *)

let is_digit ch = 
  let code = Char.code ch in
  code >= 48 && code <= 57

let is_letter ch = 
  let code = Char.code ch in
  (code >= 65 && code <= 90) || (code >= 97 && code <= 122) (* A-Z, a-z *)
 
let peek_identifier lexer =
  let rec peek_identifier' lexer acc =
    match lexer.ch with 
      | Some ch when is_alnum ch -> peek_identifier' (next_position lexer) (acc ^ String.make 1 ch)
      | _ -> lexer, acc
  in
  match peek_identifier' lexer "" with
    | lexer', "fn" -> lexer', Some Function
    | lexer', "let" -> lexer', Some Let
    | lexer', "true" -> lexer', Some True
    | lexer', "false" -> lexer', Some False
    | lexer', "if" -> lexer', Some If
    | lexer', "else" -> lexer', Some Else
    | lexer', "return" -> lexer', Some Return
    | lexer', "while" -> lexer', Some While
    | lexer', "for" -> lexer', Some For
    | lexer', "break" -> lexer', Some Break
    | lexer', "continue" -> lexer', Some Continue
    | lexer', "int" -> lexer', Some Int
    | lexer', "bool" -> lexer', Some Bool
    | lexer', ident -> lexer', Some (Identifier ident)


let peek_digit lexer = 
  let open Token in
  let rec peek_digit' lexer acc =
    match lexer.ch with
      | Some ch when is_digit ch -> peek_digit' (next_position lexer) (acc ^ String.make 1 ch)
      | _ -> lexer, Some (IntegerLit acc)
  in
  peek_digit' lexer ""

(* append everything to the accumulator until a new quote char is found*)
let peek_string_literal lexer = 
  let rec peek_string_literal' lexer acc = 
    match lexer.ch with
      | None -> lexer, Some Illegal (* string literal not closed *)
      | Some ch when ch <> '"' -> peek_string_literal' (next_position lexer) (acc ^ String.make 1 ch)
      | _ -> lexer, Some (StringLit acc)
  in
  let lexer, token = peek_string_literal' lexer ""
  in
  advance lexer, token
  (* advance the lexer to the next char after the closing quote*)

(* Returns the next lexer state and the current token *)
let next_token lexer = 
  let open Token in
  let lexer' = skip_white_space lexer in
  match lexer'.ch with
  | None -> lexer', Some EOF
  | Some ch -> 
    match ch with
      | '(' -> advance lexer',Some LeftParen 
      | ')' -> advance lexer',Some RightParen
      | '{' -> advance lexer',Some LeftBrace
      | '}' -> advance lexer',Some RightBrace
      | '+' -> advance lexer',Some Plus
      | '-' -> advance lexer',Some Minus
      | '*' -> advance lexer',Some Astherisk 
      | '/' -> advance lexer',Some Slash
      | ',' -> advance lexer',Some Comma
      | '>' -> advance lexer',Some GreaterThan
      | '<' -> advance lexer',Some LessThan
      | ';' -> advance lexer',Some Semicolon 
      | '"' -> peek_string_literal (next_position lexer')
      | '=' -> peek lexer' '=' ~whenMatched:Equal ~default:Assign
      | '!' -> peek lexer' '=' ~whenMatched:NotEqual ~default:Bang
      | ' ' | '\t' | '\n' | '\r' -> advance lexer', None
      | ch -> match ch with 
        | _ when is_letter ch -> peek_identifier lexer'
        | _ when is_digit ch -> peek_digit lexer'
        | _ -> advance lexer', Some Illegal
      (* not all token.t variants are tokenized. That's ok for this first version *)


let pp formatter lexer = 
  Format.fprintf formatter "%s\n" (show lexer)
