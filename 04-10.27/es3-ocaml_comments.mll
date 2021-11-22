{
	let n_comm = ref 0
	let n_code = ref 0
}

rule count = parse
	| "(*"	{ comments 0 lexbuf }
	| _ 	{ incr n_code; count lexbuf }
	| "*)"  { Printf.printf "Unexpected *)"; exit 1 }
	| eof	{ () }
and comments depth = parse
	| "*)"	{ if depth = 0 then count lexbuf else comments (depth-1) lexbuf }
	| _ 	{ incr n_comm; comments depth lexbuf }
	| "(*"  { comments (depth+1) lexbuf }
	| eof   { () }

{
	let () = 
		let lexbuf = Lexing.from_channel stdin in 
		count lexbuf; Printf.printf "n_comm %d, n_code %d, density %.3f\n" !n_comm !n_code (float_of_int !n_comm /. float_of_int !n_code)
}