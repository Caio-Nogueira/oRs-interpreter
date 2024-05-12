open ORun.Lexer;;
open ORun.Token;;
open Base;;

let tokenize input = 
  let lexer = init input in
  let rec tokenize' t acc = 
    match next_token t with
    | _, Some EOF -> List.rev acc
    | t', token -> tokenize' t' (token::acc) 
  in
  tokenize' lexer []

let%expect_test "lexer:trivial" =
  let input = "let x = 1;" in
  let token_opts = tokenize input in
  let l = List.filter_map ~f:Fn.id token_opts in  (* Lifts option types *)
  List.iter l ~f:(fun t -> Stdio.print_endline (ORun.Token.show t));
  [%expect {|
    Let
    (Identifier "x")
    Assign
    (IntegerLit "1")
    Semicolon |}]
  
(* TODO: function literals, expressions, nested expressions *)

let%expect_test "lexer:func" = 
  let input = "let f = fn(x) { return x; }" in
  let token_opts = tokenize input in
  let l = List.filter_map ~f:Fn.id token_opts in
  List.iter l ~f:(fun t -> Stdio.print_endline (ORun.Token.show t));
  [%expect {|
    Let
    (Identifier "f")
    Assign
    Function
    LeftParen
    (Identifier "x")
    RightParen
    LeftBrace
    Return
    (Identifier "x")
    Semicolon
    RightBrace
    |}]

let%expect_test "lexer:funcLiteral" = 
  let input = "let f = fn(x) { return fn(y) { return y; }; }" in
  let token_opts = tokenize input in
  let l = List.filter_map ~f:Fn.id token_opts in
  List.iter l ~f:(fun t -> Stdio.print_endline (ORun.Token.show t));
  [%expect {|
    Let
    (Identifier "f")
    Assign
    Function
    LeftParen
    (Identifier "x")
    RightParen
    LeftBrace
    Return
    Function
    LeftParen
    (Identifier "y")
    RightParen
    LeftBrace
    Return
    (Identifier "y")
    Semicolon
    RightBrace
    Semicolon
    RightBrace
    |}]

let%expect_test "lexer:expr" = 
  let input = "let x = 1 + 2 * 3;" in
  let token_opts = tokenize input in
  let l = List.filter_map ~f:Fn.id token_opts in
  List.iter l ~f:(fun t -> Stdio.print_endline (ORun.Token.show t));
  [%expect {|
    Let
    (Identifier "x")
    Assign
    (IntegerLit "1")
    Plus
    (IntegerLit "2")
    Astherisk
    (IntegerLit "3")
    Semicolon
    |}]

let%expect_test "lexer:expr2" =
  (* nested expression *)
  let input = "let x = 1 + (2 * 3);" in
  let token_opts = tokenize input in
  let l = List.filter_map ~f:Fn.id token_opts in
  List.iter l ~f:(fun t -> Stdio.print_endline (ORun.Token.show t));
  [%expect {|
    Let
    (Identifier "x")
    Assign
    (IntegerLit "1")
    Plus
    LeftParen
    (IntegerLit "2")
    Astherisk
    (IntegerLit "3")
    RightParen
    Semicolon
    |}]