open Ast
open Printf
open Lexing

let print_ast ast =
  Ast.affiche_program ast

let print_error lexbuf =
  let pos = lexbuf.lex_curr_p in
  eprintf "Syntax error at line %d, position %d:\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    eprintf "Error: A Rustine file name is expected.\n"
  else
    for i = 1 to argc - 1 do
      let filename = Sys.argv.(i) in
      let in_channel = open_in filename in
      let lexbuf = Lexing.from_channel in_channel in
      try
        let ast = Rustineparser.main Rustinelexer.tokenize lexbuf in
        printf "\nProcessing of %s \n\n" filename;
        printf "----|AST tree|----\n\n";
        print_ast ast;
      with
      | Parsing.Parse_error ->
        print_error lexbuf;
        close_in in_channel;
        exit 1
      | e ->
        eprintf "Error: %s\n" (Printexc.to_string e);
        close_in in_channel;
        exit 1
    done
