let rec print_list l = match l with
    | [] -> ()
    | h::t -> print_int h ; print_string " " ; print_list t ;;
    
let rec calc_row list = match list with
    | [] -> [1] 
    | [_] -> []
    | h1::h2::t -> [(h1+h2)] @ calc_row ([h2] @ t);;

let rec print_pascal (k,list) = match k with
    | 0 -> ()
    | _ -> let l = if list = [] then [1] else [1] @ calc_row list @ [1] 
            in print_list l ; print_endline("") ; print_pascal (k-1, l) ;;

let k = read_int();;
print_pascal(k,[]);;
print_endline("");;