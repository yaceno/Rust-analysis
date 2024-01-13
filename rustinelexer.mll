{
    open Rustineparser
    open Types
}

let digit = ['0'-'9']
let hex_digit = digit | ['a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z']

let identifier = (alpha | '_' ) (alpha | digit | '_')*
let identifier_precede = "r#" (alpha | '_') (alpha | digit | '_')*

let label =  "'" (alpha | '_') (alpha | digit | '_')*

(* Commentaires *)
let commentaire_ligne = "//" [^ '\n']*

(* Constantes *)
let decimal_digit = (digit | '_')*
let decimal_constant = digit decimal_digit

let binary_digit = ['0'-'1']
let binary_digit_with_underscore = binary_digit ('_' binary_digit?)*

let binary_constant = "0b" (['0'-'1'] | '_')*

let octal_constant = "0o" (['0'-'7'] | '_')*
let hexadecimal_constant = "0x" (hex_digit | '_')*

let int_suffix = "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
let float_suffix = "f32" | "f64"
let exponent = ['e' 'E'] ['+' '-']? decimal_constant


(* Constante flottante avec au moins un chiffre après le point *)
let float_constant_with_suffix = decimal_constant '.' (digit+) float_suffix? | decimal_constant exponent float_suffix?
let float_constant = decimal_constant ('.' decimal_digit)? exponent?

let typed_constant = (decimal_constant | binary_constant | octal_constant | hexadecimal_constant | float_constant_with_suffix) (int_suffix | float_suffix)?

rule tokenize = parse
    | '\n' { 
            Lexing.new_line lexbuf;
            tokenize lexbuf 
        }

    (* Commentaires, on les ignore *)
    | [' ' '\t' '\r'] { tokenize lexbuf } (* Gestion des espaces et tabulations *)
    | commentaire_ligne { tokenize lexbuf }  (* Commentaires de ligne *)
    | "/*" { comment_block lexbuf; tokenize lexbuf }  (* Commentaires de bloc *)

    | float_constant as num { TOK_FLOAT (float_of_string (String.concat "" (String.split_on_char '_' num))) }
    | decimal_constant as num { TOK_INT (int_of_string (String.concat "" (String.split_on_char '_' num))) }
    | binary_constant as num { TOK_INT (int_of_string (String.concat "" (String.split_on_char '_' num))) }
    | octal_constant as num { TOK_INT (int_of_string (String.concat "" (String.split_on_char '_' num))) }
    | hexadecimal_constant as num { TOK_INT (int_of_string (String.concat "" (String.split_on_char '_' num))) }
    | typed_constant as num { TOK_TYPED_CONST (num) }


    | "true"  { TOK_BOOL (true) }
    | "false" { TOK_BOOL (false) }

    (* Structures de contrôle *)
    | "let"   { TOK_LET } 
    | "as"    { TOK_AS }
    | "if"    { TOK_IF }
    | "else"  { TOK_ELSE }
    | "while" { TOK_WHILE }
    | "loop"  { TOK_LOOP }
    | "return" { TOK_RETURN }
    | "break" { TOK_BREAK }
    | "continue" { TOK_CONTINUE }
    | "fn" { TOK_FUNCTION }
    | ":" { TOK_COLON }
    | "->" { TOK_ARROW }
    | '+' { TOK_PLUS }
    | '-' { TOK_SUB }
    | '*' { TOK_MUL }
    | "^" { TOK_XOR }
    | '/' { TOK_DIV }
    | '(' { TOK_OPENPAREN }
    | ')' { TOK_CLOSEPAREN }
    | '{' { TOK_OPENBRACE }
    | '}' { TOK_CLOSEBRACE }
    | ';' { TOK_SEMICOLON }
    | ',' { TOK_COMMA }
    | "==" { TOK_EQ }
    | '=' { TOK_EGAL }
    | "!=" { TOK_NEQ }
    | '<' { TOK_LT }
    | "<=" { TOK_LE }
    | '>' { TOK_GT }
    | ">=" { TOK_GE }
    | ">>" { TOK_SHIFTR }
    | "<<" { TOK_SHIFTL }
    | "&&" { TOK_AND }
    | "||" { TOK_OROR }
    | '|' { TOK_OR }
    | '!' { TOK_NOT }
    | '&' { TOK_REF }
    | "mut" { TOK_MUT }
    | "%" { TOK_MOD }

    | "i8" { TOK_I8 }
    | "i16" { TOK_I16 }
    | "i32" { TOK_I32 }
    | "i64" { TOK_I64 }
    | "i128" { TOK_I128 }
    | "isize" { TOK_ISIZE }
    | "u8" { TOK_U8 }
    | "u16" { TOK_U16 }
    | "u32" { TOK_U32 }
    | "u64" { TOK_U64 }
    | "u128" { TOK_U128 }
    | "usize" { TOK_USIZE }
    | "f32" { TOK_F32 }
    | "f64" { TOK_F64 }
    | "bool" { TOK_BOOLTYPE }

    (* Mots réservés *)

    | "abstract" { TOK_ABSTRACT }
    | "async" { TOK_ASYNC }
    | "await" { TOK_AWAIT }
    | "become" { TOK_BECOME }
    | "box" { TOK_BOX }
    | "const" { TOK_CONST }
    | "crate" { TOK_CRATE }
    | "do" { TOK_DO }
    | "dyn" { TOK_DYN }
    | "enum" { TOK_ENUM }
    | "extern" { TOK_EXTERN }
    | "final" { TOK_FINAL }
    | "for" { TOK_FOR }
    | "impl" { TOK_IMPL }
    | "in" { TOK_IN }
    | "macro" { TOK_MACRO }
    | "match" { TOK_MATCH }
    | "mod" { TOK_MODR }
    | "move" { TOK_MOVE }
    | "override" { TOK_OVERRIDE }
    | "priv" { TOK_PRIV }
    | "pub" { TOK_PUB }
    | "self" { TOK_SELF }
    | "Self" { TOK_SELF }
    | "static" { TOK_STATIC }
    | "struct" { TOK_STRUCT }
    | "super" { TOK_SUPER }
    | "trait" { TOK_TRAIT }
    | "type" { TOK_TYPE }
    | "typeof" { TOK_TYPEOF }
    | "unsafe" { TOK_UNSAFE }
    | "unsized" { TOK_UNSIZED }
    | "use" { TOK_USE }
    | "virtual" { TOK_VIRTUAL }
    | "where" { TOK_WHERE }
    | "yield" { TOK_YIELD }
    | "ref" { TOK_REFRESERVED }

    | label as id { TOK_LABEL(id) }

    (* Identificateurs *)    
    | identifier as id { TOK_IDENTIFIER(id) } 
    (* On extrait l'id après r# en utilisant sub qui prend en params : la string, la position start et la longueur de la sous-string qu'on prend *)
    | identifier_precede as id { TOK_IDENTIFIER(String.sub id 2 (String.length id - 2)) }
    | eof { TOK_EOF }

and comment_block = parse
    | "*/" { () }  (* Fin du commentaire en bloc *)
    | "/*" { comment_block lexbuf; comment_block lexbuf }  (* Nouveau commentaire en bloc imbriqué *)
    | _ { comment_block lexbuf }  (* Continue d'ignorer le contenu du commentaire *)
    | eof { raise (Failure "Commentaire en bloc non terminé à la fin du fichier") }