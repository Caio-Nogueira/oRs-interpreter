type node = Program of program [@@deriving show]

and expression =
  | IntegerLit of int
  | Identifier of identifier
  | BooleanLit of bool
  | StringLit of string
  | UnaryOp of unaryOp
  | BinaryOp of binaryOp
  | IfCond of ifCond
  | FunctionLit of functionLit
  | FunctionCall of functionCall
  | ArrayLit of expression list
  | ArrayIndex of arrayIndex
  | WhileLoop of whileLoop
  | BlockExpr of block
  | NullLit
[@@deriving show { with_path = false }]

(* Program is the root node of the AST *)
(* It contains a list of statements *)

(* Null type for expressions that do not return anything *)
(* Allow to group several expressions. NOTE: Evaluation must assure that the last expression resolves into something valid*)
and program = { statements : statement list }
[@@deriving show { with_path = false }]

and statement =
  | LetStmt of letStmt
  | ReturnStmt of returnStmt
  | ExpressionStmt of expression
[@@deriving show { with_path = false }]

and letStmt =
  { identifier : identifier
  ; expression : expression
  }
[@@deriving show { with_path = false }]

and returnStmt = { expression : expression }
[@@deriving show { with_path = false }]

and binaryOp =
  { left : expression
  ; operator : Token.t
  ; right : expression
  }
[@@deriving show { with_path = false }]

and unaryOp =
  { operator : Token.t
  ; right : expression
  }
[@@deriving show { with_path = false }]

and ifCond =
  { condition : expression
  ; consequence : expression
  ; alternative : expression option
  }
[@@deriving show { with_path = false }]

and arrayIndex =
  { name : identifier
  ; index : expression
  }
[@@deriving show { with_path = false }]

and functionLit =
  { parameters : identifier list
  ; body : expression
  }
[@@deriving show { with_path = false }]

and block = { block_stmts : statement list }

and functionCall =
  { fn : identifier
  ; arguments : expression list
  }
[@@deriving show { with_path = false }]

and whileLoop =
  { condition : expression
  ; body : expression
  }
[@@deriving show { with_path = false }]

and identifier = { identifier : string }
[@@deriving show { with_path = false }]

let new_identifier name = { identifier = name }

let new_function_lit parameters block =
  { parameters; body = block }
;;

let new_function_call fn arguments = { fn; arguments }
let new_array_index name index = { name; index }
let pp fmt node = Format.fprintf fmt "[%s]" (show_node node)

let pp_program fmt program =
  Format.fprintf
    fmt
    "Program { statements = [@[<hov>%a@]]@. }"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       pp_statement)
    program.statements
;;

(* Traverser for the AST. Works like some kind of iterator *)
type traverser =
  { curr_stmt : statement option
  ; stmts : statement list
  }

let init_traverser program =
  { curr_stmt = None; stmts = program.statements }
;;

let advance_traverser traverser =
  match traverser.stmts with
  | [] -> None
  | stmt :: stmts -> Some { curr_stmt = Some stmt; stmts }
;;

let get_curr_stmt traverser = traverser.curr_stmt
