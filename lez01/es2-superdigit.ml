let rec length l = match l with
    | [] -> 0
    | h::t -> 1 + length t;;

let digits n = 
    let rec loop n acc = 
        if n = 0 then acc
        else loop (n/10) (n mod 10::acc) in
    match n with
        | 0 -> [0]
        | _ -> loop n [];;

let rec sumlist l = match l with
    | [] -> 0
    | h::t -> h + sumlist t;;

let rec super_digit d = match length(digits d) with
    | 1 -> d
    | _ -> super_digit(sumlist(digits d));;

print_int(super_digit(read_int()));;
print_endline("");;