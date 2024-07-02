open Core

(*
   TODO:
   - Arrays
   - Functions
*)

module Value : sig
  type t =
    | Null
    | Boolean of bool
    | Number of int
    | String of string
    | Error of string
  [@@deriving show { with_path = false }, ord, eq, sexp]

  val to_string : t -> string
end = struct
  type t =
    | Null
    | Boolean of bool
    | Number of int
    | String of string
    | Error of string
  [@@deriving show { with_path = false }, ord, eq, sexp]

  let to_string = function
    | Null -> "null"
    | Boolean b -> string_of_bool b
    | Number n -> string_of_int n
    | String s -> s
    | Error e -> e
  ;;
end
