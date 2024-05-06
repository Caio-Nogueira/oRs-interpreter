type node = 
  | Program of program
  | Statement of statement
  | Expression of expression

and expression = 
  | IntegerLit of int
  | Identifier of identifier
  | BooleanLit of boolean
  | StringLit of string
  | UnaryOp of unaryOp
  | BinaryOp of binaryOp
  | If of ifCond
  | FunctionLit of functionLit
  | FunctionCall of functionCall
  | ArrayLit of expression list
  | Index of arrayIndex
  | WhileLoop of whileLoop


and program = {
  statements: statement list;
}

and statement = 
  | LetStmt of letStmt
  | ReturnStmt of returnStmt
  | ExpressionStmt of expression

and letStmt = {
  identifier: identifier;
  expression: expression;
}

and returnStmt = {
  expression: expression;
}

and binaryOp = {
  left: expression;
  operator: Token.t;
  right: expression;
}

and unaryOp = {
  operator: Token.t;
  right: expression;
}

and ifCond = {
  condition: expression;
  consequence: expression;
  alternative: expression;
}

and arrayIndex = {
  array: expression;
  index: expression;
}

and functionLit = {
  parameters: identifier list;
  body: block
}

and block = {
  statements: statement list;
}

and functionCall = {
  fn: identifier;
  arguments: expression list;
}

and whileLoop = {
  condition: expression;
  body: block;
}

and identifier = {
  identifier: string;
}

and boolean = {
  boolean: bool;
}

let new_identifier name = { identifier = name }