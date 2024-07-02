open Core

(* For now, let's just save each scope inside its own environment *)

module Environment : sig
  type t

  val create : unit -> t
  val add : t -> string -> Object.Value.t -> unit
  val get : t -> string -> Object.Value.t option
  val length : t -> int
  val to_string : t -> string
end = struct
  type t = (string, Object.Value.t) Hashtbl.t

  let create () = Hashtbl.create (module String)
  let add env key value = Hashtbl.set env ~key ~data:value
  let get env key = Hashtbl.find env key
  let length env = Hashtbl.length env

  let to_string env =
    Hashtbl.to_alist env
    |> List.map ~f:(fun (key, value) ->
      key ^ " = " ^ Object.Value.to_string value)
    |> String.concat ~sep:", "
  ;;
end
