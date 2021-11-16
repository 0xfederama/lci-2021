let rec list_replication l n = match n with
    | 0 -> []
    | _ -> l @ list_replication l (n-1);;

let rec print_list l = match l with
    | [] -> ()
    | h::t -> print_int h ; print_string ";" ; print_list t ;;

let l = list_replication [1;2;3;4] 3;;
print_list(l);;
print_endline("");;