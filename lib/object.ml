open Core

(*
   TODO:
   - Arrays
   - Functions
*)

module Value : sig
  type t =
    | Nil
    | Boolean of bool
    | Number of int
    | String of string
  [@@deriving show { with_path = false }, ord, eq, sexp]
end = struct
  type t =
    | Nil
    | Boolean of bool
    | Number of int
    | String of string
  [@@deriving show { with_path = false }, ord, eq, sexp]
end
