%{
  open Ast
  open Types
%}

%token <string> TOK_TYPED_CONST
%token <int> TOK_INT
%token TOK_I8 TOK_I16 TOK_I32 TOK_I64 TOK_I128 TOK_ISIZE TOK_U8 TOK_U16 TOK_U32 TOK_U64 TOK_U128 TOK_USIZE TOK_F32 TOK_F64 TOK_BOOLTYPE TOK_UNIT
%token <float> TOK_FLOAT
%token <string> TOK_BIN TOK_OCT TOK_HEX
%token <string * string> TOK_TYPED_INT TOK_TYPED_FLOAT
%token <bool> TOK_BOOL
%token <string> TOK_IDENTIFIER
%token <string> TOK_LABEL
%token TOK_FUNCTION
%token TOK_WHILE TOK_RETURN TOK_LET TOK_AS TOK_IF TOK_ELSE TOK_LOOP TOK_BREAK TOK_CONTINUE TOK_LABEL
%token TOK_PLUS TOK_SUB TOK_MUL TOK_DIV TOK_MOD TOK_MODR TOK_XOR
%token TOK_NEQ TOK_EQ TOK_AND TOK_OROR TOK_OR TOK_LT TOK_LE TOK_GT TOK_GE TOK_SHIFTR TOK_SHIFTL
%token TOK_OPENPAREN TOK_CLOSEPAREN TOK_OPENBRACE TOK_CLOSEBRACE TOK_SEMICOLON TOK_COMMA TOK_COLON TOK_EOF
%token TOK_EGAL TOK_NOT TOK_REF TOK_MUT TOK_ARROW

// Reserved words

%token TOK_REFRESERVED TOK_ABSTRACT TOK_ASYNC TOK_AWAIT TOK_BECOME TOK_BOX TOK_CONST TOK_CRATE TOK_DO TOK_DYN TOK_ENUM TOK_EXTERN TOK_FINAL TOK_FOR TOK_IMPL TOK_IN TOK_MACRO TOK_MATCH TOK_MOVE TOK_OVERRIDE TOK_PRIV TOK_PUB TOK_SELF TOK_STATIC TOK_STRUCT TOK_SUPER TOK_TRAIT TOK_TYPE TOK_TYPEOF TOK_UNSAFE TOK_UNSIZED TOK_USE TOK_VIRTUAL TOK_WHERE TOK_YIELD

%nonassoc TOK_ELSE TOK_FN TOK_LOOP TOK_WHILE TOK_IF TOK_LOOP TOK_OPENPAREN TOK_CLOSEPAREN TOK_ARROW TOK_LBRACE TOK_RBRACE TOK_SEMICOLON TOK_COMMA TOK_BOOL TOK_FOR
%right TOK_NEG TOK_NOT TOK_DEREF TOK_MUT  // Opérateurs unaires classiques
%right TOK_REF  // Référence (&) et référence mutable (&mut)
%left TOK_AS  
%left TOK_MUL TOK_DIV TOK_MOD         /* Opérations multiplicatives */
%left TOK_ADD TOK_SUB              /* Opérations additives */
%left TOK_SHIFTL TOK_SHIFTR
%left TOK_XOR
%left TOK_OR                      /* Opérateur bit à bit | */              
%left TOK_AND                    /* Opération logique && */
%left TOK_OROR                    /* Opération logique || */
%right TOK_EGAL TOK_LET TOK_COLON
%nonassoc TOK_RETURN TOK_BREAK TOK_CONTINUE
%nonassoc TOK_LT TOK_LE TOK_GT TOK_GE TOK_EQ TOK_NEQ /* Comparaisons */    


%start main
%type <Types.function_ list> main
%type <Types.expression> expr
%type <Types.declaration> decl
%type <Types.function_> function_
%type <(bool * string * Types.type_) list> param_list
%type <(bool * string * Types.type_)> param

%%



/* Basic types */

type_simple:
    | TOK_I8 { I8 }
    | TOK_I16 { I16 }
    | TOK_I32 { I32 }
    | TOK_I64 { I64 }
    | TOK_I128 { I128 }
    | TOK_ISIZE { Isize }
    | TOK_U8 { U8 }
    | TOK_U16 { U16 }
    | TOK_U32 { U32 }
    | TOK_U64 { U64 }
    | TOK_U128 { U128 }
    | TOK_USIZE { Usize }
    | TOK_F32 { F32 }
    | TOK_F64 { F64 }
    | TOK_BOOLTYPE { Bool }

type_complexe:
    | TOK_OPENPAREN TOK_CLOSEPAREN { Unit }
    | TOK_REF mut_opt type_ { Mut $3 }
    | TOK_OPENPAREN type_ TOK_CLOSEPAREN { $2 }

type_:
    | type_simple { $1 }
    | type_complexe { $1 }
;

/* Expressions */

primary_expr:
    | TOK_INT { ConstINT $1 }
    | TOK_FLOAT { ConstFLOAT $1 }
    | TOK_TYPED_CONST { ConstTYPED $1 }
    | TOK_BOOL { ConstBOOL $1 }
    | TOK_LABEL { Label $1 }
    | TOK_IDENTIFIER { Identifiant $1 }
    | TOK_OPENPAREN expr TOK_CLOSEPAREN { Parenexpr $2 }
;

unary_expr:
    | TOK_NOT primary_expr { UnaryOp(NOT, $2) }
    | TOK_REF mut_opt expr { 
        match $2 with
        | true -> UnaryOp(MUTREF, $3)
        | false -> UnaryOp(REF, $3)
    }
    | TOK_MUT primary_expr { UnaryOp(MUT, $2) }
    | TOK_MUL unary_expr { UnaryOp(DEREF, $2) }
    | TOK_MUL primary_expr { UnaryOp(DEREF, $2) }
    | TOK_SUB primary_expr { UnaryOp(NEG, $2) }
;

binary_expr:
    | expr_sans_bloc TOK_PLUS expr_sans_bloc { BinaryOp(PLUS, $1, $3) }
    | expr_sans_bloc TOK_SUB expr_sans_bloc { BinaryOp(SUB, $1, $3) }
    | expr_sans_bloc TOK_MUL expr_sans_bloc { BinaryOp(MUL, $1, $3) }
    | expr_sans_bloc TOK_DIV expr_sans_bloc { BinaryOp(DIV, $1, $3) }
    | expr_sans_bloc TOK_EQ expr_sans_bloc { BinaryOp(EQ, $1, $3) }
    | expr_sans_bloc TOK_EGAL expr_sans_bloc { BinaryOp(EGAL, $1, $3) }
    | expr_sans_bloc TOK_OROR expr_sans_bloc { BinaryOp(OROR, $1, $3) }
    | expr_sans_bloc TOK_OR expr_sans_bloc { BinaryOp(OR, $1, $3) }
    | expr_sans_bloc TOK_LT expr_sans_bloc { BinaryOp(LT, $1, $3) }
    | expr_sans_bloc TOK_LE expr_sans_bloc { BinaryOp(LE, $1, $3) }
    | expr_sans_bloc TOK_GT expr_sans_bloc { BinaryOp(GT, $1, $3) }
    | expr_sans_bloc TOK_GE expr_sans_bloc { BinaryOp(GE, $1, $3) }
    | expr_sans_bloc TOK_AND expr_sans_bloc { BinaryOp(AND, $1, $3) }
    | expr_sans_bloc TOK_NEQ expr_sans_bloc { BinaryOp(NEQ, $1, $3) }
    | expr_sans_bloc TOK_MOD expr_sans_bloc { BinaryOp(MOD, $1, $3) }
    | expr_sans_bloc TOK_XOR expr_sans_bloc { BinaryOp(XOR, $1, $3) }
    | expr_sans_bloc TOK_REF expr_sans_bloc { BinaryOp(ANDBINAIRE, $1, $3) }
    | expr_sans_bloc TOK_SHIFTL expr_sans_bloc { BinaryOp(SHIFTL, $1, $3) }
    | expr_sans_bloc TOK_SHIFTR expr_sans_bloc { BinaryOp(SHIFTR, $1, $3) }
;

label_opt:
    | TOK_LABEL TOK_COLON { Some $1 }
    | /* empty */ { None }

else_opt:
    | TOK_ELSE bloc { Some $2 }
    | /* empty */ { None }

expr_bloc:    
    | TOK_IF expr bloc else_opt { If($2, $3, $4) }
    | label_opt TOK_LOOP bloc { Loop($1, $3) }
    | label_opt TOK_WHILE expr bloc { While($3, $4) }
    | label_opt TOK_OPENBRACE decl_list expr_list TOK_CLOSEBRACE { BlocExpression($3, $4) };
;

args_opt:
    | expr_list { $1 }
;

etiq_opt:
    | /* empty */ { None }
    | TOK_LABEL { Some $1 }
;

expr_opt:
    | /* empty */ { None }
    | expr_sans_bloc { Some $1 }

expr_sans_bloc:
    | primary_expr { $1 }
    | unary_expr { $1 }
    | binary_expr { $1 }
    | TOK_IDENTIFIER TOK_EGAL expr { Assign($1, $3) }
    | expr_sans_bloc TOK_AS type_ { As($1, $3) }
    | TOK_BREAK etiq_opt expr_opt { Break($2, $3) }
    | TOK_CONTINUE etiq_opt { Continue($2) }
    | TOK_RETURN expr { Return( Some $2) }
    | TOK_RETURN { Return(None) }
    | TOK_IDENTIFIER TOK_OPENPAREN args_opt TOK_CLOSEPAREN { Call($1, $3) }
;

expr:
    | expr_bloc { $1 }
    | expr_sans_bloc { $1 }

/* Déclarations */

type_opt:
    | /* empty */ { None }
    | TOK_COLON type_ { Some $2 }
;

init_opt:
    | /* empty */ { None }
    | TOK_EGAL expr { Some $2 }
    | TOK_REF expr { Some (UnaryOp(MUTREF, $2)) }
    | TOK_MUL expr { Some (UnaryOp(DEREF, $2)) }
;

mut_opt:
    | /* empty */ { false }
    | TOK_MUT { true }
;

decl:
    | TOK_LET mut_opt TOK_IDENTIFIER type_opt init_opt TOK_SEMICOLON { Let($2, $3, $4, $5) }
    | TOK_SEMICOLON { EmptyDeclaration }
    | expr TOK_SEMICOLON { ExprDecl($1) }
    | function_ { Funcdecl($1) }
;


decl_list:
  | decl_list decl { $2 :: $1 }
  | /* empty */ { [] }
;


/* Blocs de code */

expr_list:
    | expr_list expr { $2 :: $1 }  // Liste d'expressions sans point-virgule final
    | expr { [$1] }  // Expression seule
    | expr_list TOK_COMMA expr { $3 :: $1 }  // Liste d'expressions séparées par des virgules
    | /* empty */ { [] }
;

bloc:
    | label_opt TOK_OPENBRACE decl_list expr_list TOK_CLOSEBRACE {
        MixedBloc($1, $3, $4) 
    }
;

/* Paramètres et fonctions */

param:
    | mut_opt TOK_IDENTIFIER TOK_COLON type_ { ($1, $2, $4) }
;

param_list:
    | /* empty */ { [] }
    | param_list_nonempty { $1 }
;

param_list_nonempty:
    | param { [$1] }
    | param_list_nonempty TOK_COMMA param { $3 :: $1 }
    | param_list_nonempty TOK_COMMA { $1 }  // Accepte une virgule après le dernier paramètre

function_:
  | TOK_FUNCTION TOK_IDENTIFIER TOK_OPENPAREN param_list TOK_CLOSEPAREN bloc {
      Function ($2, $4, None, $6)
    }
  | TOK_FUNCTION TOK_IDENTIFIER TOK_OPENPAREN param_list TOK_CLOSEPAREN TOK_ARROW type_ bloc {
      Function ($2, $4, Some $7, $8)
    }
;

function_list:
    | function_ { [$1] }
    | function_list function_ { $2 :: $1 }
;


/* Point d'entrée principal */
main:
    | function_list TOK_EOF { $1 }
;
