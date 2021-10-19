(* Utility functions *)
let rec fact a = match a with
    | 0 -> 1
    | 1 -> a
    | _ -> a*fact(a-1);;

let rec pow (a,p) = match p with
    | 0 -> 1
    | 1 -> 1
    | _ -> a*pow(a, p-1);;

(* Expansion of e^x 10 times *)
let rec expansion (x,times) = match times with
    | 0 -> 1.0
    | _ -> ((x ** float_of_int(times)) /. (float_of_int(fact(times)))) +. expansion(x,times-1);;

print_float(expansion(float_of_int(read_int()),10));;
print_endline("");;