{}

let dot = '.'
let digit = ['0' - '9']
let exponent = ['e' 'E']
let sign = ['+' '-']
let float = '-'? digit+ (dot digit*)? (exponent sign? digit*)?
let whitespace = ' ' | '\n' | '\r' | '\t' | "\r\n"

rule token = parse
	| whitespace	{ token lexbuf }
	| float as n 	{ Printf.eprintf "%.4f\n" (float_of_string n); token lexbuf }
	| _ as c		{ Printf.eprintf "Unknown char %c\n" c; exit 1 }
	| eof 			{ () }

{
	let () = 
		let lexbuf = Lexing.from_channel stdin in 
		token lexbuf
}
