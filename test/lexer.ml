open ORun.Lexer;;

let tokenize input = 
  let lexer = init input in
  let rec tokenize' t acc = 
    match next_token t with
    | _, Some EOF -> List.rev acc
    | t', token-> tokenize' t' (token::acc) 
  in
  tokenize' lexer []

let test_lexer filename =
  let input_channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line input_channel in
      read_lines (acc ^ line ^ "\n")
    with End_of_file ->
      close_in input_channel;
      acc
    in 
    let token_opts = tokenize (read_lines "") in
    let l = List.filter_map Fun.id token_opts (* Fun.id = (a' -> a') *)
    in
    pp_tokens l


let get_project_root () = 
  let current_file_dir = Unix.realpath "."  in
  let dirs = String.split_on_char '/' current_file_dir in
  let rec find_project_root dirs = 
    match dirs with
    | [] -> failwith "Could not find project root"
    | hd::tl -> 
      if hd = "oRun" then 
        String.concat "/" (List.rev tl)
      else 
        find_project_root tl
  in
  find_project_root (List.rev dirs)

let () =
  let root = get_project_root () ^ "/oRun/test/inputs/hello_world.ors" in
  test_lexer root
