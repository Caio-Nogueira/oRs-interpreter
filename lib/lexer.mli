(** This interface file defines the visible types and methods required to tokenize 
   Lexer receives a string and returns a list of tokens 
*)

(** t is the lexer type *)
type t

(** Init starts the tokenizer with the full input string*)
val init : string -> t

(** Iterates the lexer - returns a new lexer with the next state and an optional token *)
val next_token : t -> t * Token.t option

(** pretty print the*)
val pp : Format.formatter -> t -> unit

