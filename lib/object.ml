open Core

(*
   TODO:
   - Arrays
   - Functions
   - Errors as values
*)

module Value : sig
  type t =
    | Null
    | Boolean of bool
    | Number of int
    | String of string
    | Error of string
  [@@deriving show { with_path = false }, ord, eq, sexp]
end = struct
  type t =
    | Null
    | Boolean of bool
    | Number of int
    | String of string
    | Error of string
  [@@deriving show { with_path = false }, ord, eq, sexp]
end
