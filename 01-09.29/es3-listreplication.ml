let rec elem_replication i n = match n with
    | 1 -> [i]
    | _ -> [i] @ elem_replication  i (n-1);;

let rec list_replication l n = match l with
    | [] -> []
    | h::t -> elem_replication h n @ list_replication t n;;

(* let rec list_replication l n = match n with
    | 0 -> []
    | 1 -> l
    | _ -> match l with
        | [] -> []
        | h::t -> elem_replication h n @ list_replication t n;; *)

let rec print_list l = match l with
    | [] -> ()
    | h::t -> print_int h ; print_string ";" ; print_list t ;;

let l = list_replication [1;2;3;4] 5;;
print_list(l);;
print_endline("");;