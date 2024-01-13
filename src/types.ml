(**  Partie Types **)

type type_ =
  | I8 | I16 | I32 | I64 | I128 | Isize
  | U8 | U16 | U32 | U64 | U128 | Usize
  | F32 | F64
  | Bool
  | Unit
  | Ref of type_
  | Mut of type_
  | MutRef of type_

and unary_op = 
  | REF | MUT | MUTREF | DEREF | NEG | NOT 

and binary_op = 
  | PLUS | SUB | MUL | DIV | MOD
  | AND | OR | XOR | SHIFTL | SHIFTR
  | EGAL |  EQ | NEQ | LT | LE | GT | GE
  | OROR | ANDBINAIRE

and label = string

and expression = 
  | ConstINT of int
  | ConstFLOAT of float
  | ConstBOOL of bool
  | ConstTYPED of string
  | Identifiant of string
  | Label of string
  | UnaryOp of unary_op * expression
  | BinaryOp of binary_op * expression * expression
  | Assign of string * expression
  | Parenexpr of expression
  | If of expression * bloc * bloc option
  | As of expression * type_
  | While of expression * bloc
  | Loop of label option * bloc
  | For of expression * expression * expression * bloc
  | Return of expression option
  | ReturnBloc of bloc
  | Call of string * expression list
  | Break of label option * expression option
  | Continue of label option
  | BlocExpression of declaration list * expression list
  | EmptyExpression

and bloc =
  | EmptyBloc
  | DeclarationBloc of label option * declaration list
  | ExpressionBloc of label option * expression list
  | MixedBloc of label option * declaration list * expression list

and function_ =
  | Function of string * ((bool * string * type_) list) * type_ option * bloc  

and declaration =
  | EmptyDeclaration
  | Let of bool * string * type_ option * expression option  (* bool pour indiquer si il y a un "mut" *)
  | Funcdecl of function_
  | ExprDecl of expression
