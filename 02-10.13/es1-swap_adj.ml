let rec swap_adjacent l = match l with
    | [] -> []
    | h::[] -> [h]
    | h1::h2::t -> [h2] @ [h1] @ swap_adjacent t;;

let s = "abcdpqr";;
let list = List.init (String.length s) (String.get s);;
print_string(String.of_seq (List.to_seq (swap_adjacent list)));;
print_endline("");;
