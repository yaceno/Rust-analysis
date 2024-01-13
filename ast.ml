open Types


let rec print_sep l = 
  match l with
  | [] -> ()
  | [x] -> Printf.printf "%s" x
  | x :: xs -> Printf.printf "%s, " x; print_sep xs

let string_of_unary_op = function
  | REF -> "REF"
  | MUT -> "MUT"
  | MUTREF -> "MUTREF"
  | DEREF -> "DEREF"
  | NEG -> "NEG"
  | NOT -> "NOT"

let string_of_binary_op = function
  | PLUS -> "PLUS"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | AND -> "AND"
  | OR -> "OR"
  | XOR -> "XOR"
  | SHIFTL -> "SHL"
  | SHIFTR -> "SHR"
  | EGAL -> "EGAL"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LE -> "LE"
  | GT -> "GT"
  | GE -> "GE"
  | OROR -> "OROR"
  | ANDBINAIRE -> "ANDBINAIRE"

let rec repeat_string s n =
  if n <= 0 then "" else s ^ (repeat_string s (n - 1))

let indent_level = 2
let increase_indent indent = indent ^ (repeat_string " " indent_level)

let rec affiche_expression indent = function
  | ConstINT i -> Printf.printf "%sConstINT(%d)\n" indent i
  | ConstFLOAT f -> Printf.printf "%sConstFLOAT(%g)\n" indent f  
  | ConstTYPED t -> Printf.printf "%sConstTYPED(%s)\n" indent t
  | ConstBOOL b -> Printf.printf "%sConstBOOL(%b)\n" indent b
  | Identifiant id -> Printf.printf "%sIdentifiant(%s)\n" indent id
  | UnaryOp (op, expr) ->
      Printf.printf "%sUnaryOp(%s)\n" indent (string_of_unary_op op);
      affiche_expression (increase_indent indent) expr
  | BinaryOp (op, expr1, expr2) ->
      Printf.printf "%sBinaryOp(%s)\n" indent (string_of_binary_op op);
      affiche_expression (increase_indent indent) expr1;
      affiche_expression (increase_indent indent) expr2
  | Assign (id, expr) ->
      Printf.printf "%sAssign to %s\n" indent id;
      affiche_expression (increase_indent indent) expr
  | Parenexpr expr ->
      Printf.printf "%sParenexpr\n" indent;
      affiche_expression (increase_indent indent) expr
  | If (expr, bloc1, bloc2_opt) ->
      Printf.printf "%sIf\n" indent;
      affiche_expression (increase_indent indent) expr;
      Printf.printf "%sThen\n" indent;
      affiche_bloc (increase_indent indent) bloc1;
      (match bloc2_opt with
        | Some bloc2 -> 
            Printf.printf "%sElse\n" indent;
            affiche_bloc (increase_indent indent) bloc2
        | None -> ())
  | As (expr, t) ->
      Printf.printf "%sAs\n" indent;
      affiche_expression (increase_indent indent) expr;
      Printf.printf "%s  To Type: " indent;
      affiche_type (increase_indent indent) t;
      Printf.printf "\n"
  | While(expr, bloc) ->
      Printf.printf "%sWhile Loop:\n" indent;
      Printf.printf "%s  Condition: " indent;
      affiche_expression (increase_indent indent) expr;
      Printf.printf "%s  Body:\n" indent;
      affiche_bloc (increase_indent indent) bloc
  | Loop (label_opt, bloc) ->
      Printf.printf "%sLoop\n" indent;
      (match label_opt with
        | Some label -> Printf.printf "%s  Label: %s\n" indent label
        | None -> ());
      affiche_bloc (indent ^ "  ") bloc
  | For (init_expr, cond_expr, post_expr, bloc) ->
      Printf.printf "%sFor Loop:\n" indent;
      Printf.printf "%s  Init Expr: " indent;
      affiche_expression (increase_indent indent) init_expr;
      Printf.printf "%s  Condition Expr: " indent;
      affiche_expression (increase_indent indent) cond_expr;
      Printf.printf "%s  Post Expr: " indent;
      affiche_expression (increase_indent indent) post_expr;
      Printf.printf "%s  Body:\n" indent;
      affiche_bloc (increase_indent indent) bloc
  | Return expr_opt ->
      Printf.printf "%sReturn\n" indent;
      (match expr_opt with
       | Some expr -> affiche_expression (increase_indent indent) expr
       | None -> ())
  | Call (func_name, args) ->
      Printf.printf "%sCall to %s with args: \n" indent func_name;
      List.iter (fun arg -> affiche_expression (indent ^ "  ") arg) args
  | Break (label_opt, expr_opt) ->
      Printf.printf "%sBreak\n" indent;
      (match label_opt with
       | Some label -> Printf.printf "%s  Label: %s\n" indent label
       | None -> ());
      (match expr_opt with
       | Some expr -> affiche_expression (increase_indent indent) expr
       | None -> ())
  | Continue label_opt ->
      Printf.printf "%sContinue\n" indent;
      (match label_opt with
       | Some label -> Printf.printf "%s  Label: %s\n" indent label
       | None -> ())
  | BlocExpression (decls, exprs) ->
      Printf.printf "%sBlocExpression:\n" indent;
      List.iter (fun decl -> affiche_declaration (increase_indent indent) decl) (List.rev decls);
      List.iter (fun expr -> affiche_expression (increase_indent indent) expr) (List.rev exprs)
  | _ -> ()


and affiche_bloc indent = function
  | EmptyBloc -> 
      Printf.printf "%sEmptyBloc\n" indent
  | DeclarationBloc (_, decls) ->
      Printf.printf "%sDeclarationBloc:\n" indent;
      List.iter (fun decl -> affiche_declaration (increase_indent indent) decl) (List.rev decls)
  | ExpressionBloc (_, exprs) ->
      Printf.printf "%sExpressionBloc:\n" indent;
      List.iter (fun expr -> affiche_expression (increase_indent indent) expr) (List.rev exprs)
  | MixedBloc (_, decls, exprs) when decls = [] && exprs = [] ->
      Printf.printf "%sEmptyBloc\n" indent
  | MixedBloc (_, decls, exprs) when decls = [] ->
      Printf.printf "%sExpressionBloc:\n" indent;
      List.iter (fun expr -> affiche_expression (increase_indent indent) expr) (List.rev exprs)
  | MixedBloc (_, decls, exprs) when exprs = [] ->
      Printf.printf "%sDeclarationBloc:\n" indent;
      List.iter (fun decl -> affiche_declaration (increase_indent indent) decl) (List.rev decls)
  | MixedBloc (_, decls, exprs) ->
      Printf.printf "%sMixedBloc:\n" indent;
      List.iter (fun decl -> affiche_declaration (increase_indent indent) decl) (List.rev decls);
      List.iter (fun expr -> affiche_expression (increase_indent indent) expr) (List.rev exprs)

and affiche_declaration indent = function
  | EmptyDeclaration -> Printf.printf "%sEmptyDeclaration\n" indent
  | Let (is_mut, name, type_opt, expr_opt) ->
    let mut_str = if is_mut then "mut " else "" in
    Printf.printf "%sLet %s%s: " indent mut_str name;
    (match type_opt with
     | Some t -> affiche_type (increase_indent indent) t; Printf.printf " = "
     | None -> ());
    (match expr_opt with
     | Some e -> affiche_expression (increase_indent indent) e
     | None -> ());
    Printf.printf "\n"
  | Funcdecl f -> affiche_function indent f
  | ExprDecl expr -> affiche_expression indent expr


and affiche_type indent = function
  | I8 -> Printf.printf "%sI8\n" indent
  | I16 -> Printf.printf "%sI16\n" indent
  | I32 -> Printf.printf "%sI32\n" indent
  | I64 -> Printf.printf "%sI64\n" indent
  | I128 -> Printf.printf "%sI128\n" indent
  | Isize -> Printf.printf "%sISIZE\n" indent
  | U8 -> Printf.printf "%sU8\n" indent
  | U16 -> Printf.printf "%sU16\n" indent
  | U32 -> Printf.printf "%sU32\n" indent
  | U64 -> Printf.printf "%sU64\n" indent
  | U128 -> Printf.printf "%sU128\n" indent
  | Usize -> Printf.printf "%sUSIZE\n" indent
  | F32 -> Printf.printf "%sF32\n" indent
  | F64 -> Printf.printf "%sF64\n" indent
  | Bool -> Printf.printf "%sBOOL\n" indent
  | Ref t -> Printf.printf "%sREF\n" indent; affiche_type (increase_indent indent) t
  | MutRef t -> Printf.printf "%sMUTREF\n" indent; affiche_type (increase_indent indent) t
  | Mut t -> Printf.printf "%sMUT\n" indent; affiche_type (increase_indent indent) t
  | Unit -> Printf.printf "%sUNIT\n" indent


and affiche_function indent (Function (name, params, return_type, body)) =
  Printf.printf "%sFunction %s:\n" indent name;
  Printf.printf "%sParameters:\n" indent;
  List.iter (fun (is_mut, param_name, param_type) -> (* Ajout de is_mut *)
    let mut_str = if is_mut then "mut " else "" in (* Gestion de l'affichage de mutabilité *)
    Printf.printf "%s%s%s: " (increase_indent indent) mut_str param_name;
    affiche_type (increase_indent (increase_indent indent)) param_type) (List.rev params);
  (match return_type with
    | Some t -> Printf.printf "%sReturn type: " indent; affiche_type (increase_indent indent) t
    | None -> ());
  Printf.printf "%sBody:\n" indent;
  affiche_bloc (increase_indent indent) body


let affiche_program functions =
  List.iter (fun f -> affiche_function "" f) (List.rev functions)