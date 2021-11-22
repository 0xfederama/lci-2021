{
type token_type =
    | ELSE
    | EQUAL
    | FALSE
    | FUN
    | LESS
    | LET
    | ID of string
    | IF
    | IN
    | INT of int
    | MINUS
    | PLUS
    | STAR
    | THEN
    | TRUE

type token = { token_t: token_type; position: Lexing.position }

let make_token token_t lexbuf = { token_t = token_t; position = Lexing.lexeme_start_p lexbuf }

let string_of_token = function
    | ELSE              -> "ELSE"
    | EQUAL             -> "EQUAL"
    | FALSE             -> "FALSE"
    | FUN               -> "FUN"
    | LESS              -> "LESS"
    | LET               -> "LET"
    | ID(id)            -> Printf.sprintf "ID(%s)" id
    | IF                -> "IF"
    | IN                -> "IN"
    | INT(i)            -> Printf.sprintf "INT(%d)" i        
    | MINUS             -> "MINUS"
    | PLUS              -> "PLUS"
    | STAR              -> "STAR"
    | THEN              -> "THEN"
    | TRUE              -> "TRUE"

let string_of_position p =
    let line_number = p.Lexing.pos_lnum in
    let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
    Printf.sprintf "(%d, %d)" line_number column

let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

let keyword_table =
    create_hashtable 6 [
        ("else", ELSE);
        ("false", FALSE);
        ("fun", FUN);
        ("let", LET);
        ("if", IF);
        ("in", IN);
        ("then", THEN);
        ("true", TRUE);
    ]

}

let digit = [ '0'-'9' ]
let number = digit+
let letter = [ 'a'-'z' 'A'-'Z' ]
let id = letter ( digit | letter )* 

rule scanner = parse
    | number as n   { make_token (INT(int_of_string n)) lexbuf }
    | id as word    {
											try 
													make_token (Hashtbl.find keyword_table word) lexbuf
											with Not_found ->
													make_token (ID word) lexbuf
										}
    | '+'           { make_token PLUS lexbuf }
    | '-'           { make_token MINUS lexbuf }
    | '*'           { make_token STAR lexbuf }
    | '='           { make_token EQUAL lexbuf }
    | '<'           { make_token MINUS lexbuf }
    | [ ' ' '\t' ]  { scanner lexbuf } (* remove whitespaces *)
    | '\n'          { Lexing.new_line lexbuf; scanner lexbuf }
    | _ as c        { 
											let position = Lexing.lexeme_start_p lexbuf in
											Printf.printf "Unrecognised character '%c' @ %s\n" c (string_of_position position);
											scanner lexbuf 
                    }                    
    | eof           { Printf.printf "EOF"; raise End_of_file }

{
let rec parse lexbuf =
    let token = scanner lexbuf in
        Printf.printf "%s @ %s\n" (string_of_token token.token_t) (string_of_position token.position); 
        parse lexbuf

let () =
     let cin = 
        if Array.length Sys.argv > 1 then
            open_in Sys.argv.(1)
        else
            stdin
    in
    let lexbuf = Lexing.from_channel cin in
    try
        parse lexbuf
    with End_of_file -> () 
}