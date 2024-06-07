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

(* Null type for expressions that do not return anything *)
(* Allow to group several expressions. NOTE: Evaluation must assure that the last expression resolves into something valid*)
and program = { statements : statement list }

and statement =
  | LetStmt of letStmt
  | ReturnStmt of returnStmt
  | ExpressionStmt of expression

and letStmt =
  { identifier : identifier
  ; expression : expression
  }

and returnStmt = { expression : expression }

and binaryOp =
  { left : expression
  ; operator : Token.t
  ; right : expression
  }

and unaryOp =
  { operator : Token.t
  ; right : expression
  }

and ifCond =
  { condition : expression
  ; consequence : expression
  ; alternative : expression option
  (* None if else statement does not exist *)
  }

and arrayIndex =
  { name : identifier
  ; index : expression
  }

and functionLit =
  { parameters : identifier list
  ; body : expression
  }

and block = { block_stmts : statement list }

and functionCall =
  { fn : identifier
  ; arguments : expression list
  }

and whileLoop =
  { condition : expression
  ; body : expression
  }

and identifier = { identifier : string } [@@deriving show]

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
