{
    open Lexing
    open Parser

    exception SyntaxError of string

    let next_line lexbuf =
      let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                   pos_lnum = pos.pos_lnum + 1
        }
}


let natural = ['0'-'9']+

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule read = parse
    | whitespace { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }

    | '(' { LEFT_PARENTHESIS }
    | ')' { RIGHT_PARENTHESIS }

    | "bool" { BOOL_TYPE }
    | "nat" { NATURAL_TYPE }

    | "function" { FUNCTION }
    | ":" { COLON }
    | "->" { ARROW }

    | natural { NATURAL (int_of_string (Lexing.lexeme lexbuf)) }
    | '+' { PLUS }
    | '*' { STAR }
    | '-' { MINUS }
    | '/' { SLASH }
    | '=' { EQUAL }

    | "true" { TRUE }
    | "false" { FALSE }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }

    | "fix" { FIX }

    | "exception" { EXCEPTION }
    | "of" { OF }
    | "in" { IN }
    | "raise" { RAISE }
    | "as" { AS }
    | "try" { TRY }
    | "catch" { CATCH }
    | "|" { PIPE }

    | id { ID (Lexing.lexeme lexbuf) }

    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

    | eof { END_OF_FILE }
