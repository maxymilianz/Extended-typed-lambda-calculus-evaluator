open Core
open Lexer
open Lexing


let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let parse_with_error lexbuf =
    try Parser.parse_program Lexer.read lexbuf with
    | SyntaxError msg ->
        fprintf stderr "%a: %s\n" print_position lexbuf msg;
        None
    | Parser.Error ->
        fprintf stderr "%a: syntax error\n" print_position lexbuf;
        exit (-1)


let parse lexbuf =
    match parse_with_error lexbuf with
    | None -> failwith "Couldn't parse program."
    | Some expression -> expression


let parse_string value =
    let lexbuf = Lexing.from_string value in
    parse lexbuf
