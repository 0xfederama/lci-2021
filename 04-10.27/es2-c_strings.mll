{}

rule strings = parse
  | "\\n"     { Printf.printf "\n"; token lexbuf }
  | "\\t"     { Printf.printf "\t"; token lexbuf }
  | "\\'"     { Printf.printf "'"; token lexbuf }
  | "\\\""    { Printf.printf "\""; token lexbuf }
  | _ as c	{ Printf.printf "%c" c; token lexbuf}
  | eof		{ () }

{
	let () = 
		let lexbuf = Lexing.from_channel stdin in 
		strings lexbuf
}