(** Expressible and denotable values *)
type value =
    | None
    | Int of int
    | String of string
    | Tuple of value list * int
    | Lista of value list

(** Syntax of the language *)
type expr =
    | CstI of int
    | CstB of bool
    | CstS of string
    | IfElse of expr * expr * expr
    | Proj of value * int

(** Typechecker *)
let typecheck ((x, y) : (string * value)) : bool = match x with
    | "none" -> if y == None then true else false
    | "int" -> (match y with
        | Int y -> true
        | _ -> false
    )
    | "string" -> (match y with
        | String y -> true
        | _ -> false
    )
    | "tuple" -> (match y with
        | Tuple (list, len) -> true
        | _ -> false
    )
    | "list" -> (match y with
        | Lista y -> true
        | _ -> false
    )
    | _ -> failwith ("Invalid type " ^ x)

(* Functions *)
let rec tuple_proj (tuple, index) : value = match typecheck("tuple", tuple), tuple, typecheck("int", index), index with
    | false, _, _, _ -> failwith ("Invalid type: tuple must be a tuple (value list * int)")
    | _, _, false, _ -> failwith ("Invalid type: index must be an integer")
    | true, Tuple([], _), true,  _ -> None
    | true, Tuple(h::t, len), true, Int(0) -> h
    | true, Tuple(h::t, len), true, Int(i) -> tuple_proj (Tuple(t, len), Int(i-1))
    | _, _, _, _ -> failwith "runtime error"

(** Interpreter for expression. Given an expression {e} and an enviroment {env} that closes {e},
    this function evaluates {e} and returns the result of the computation.
    Note this function implements the big-step operational semantics with environment.
 *)
let rec eval (e : expr) : value = match e with
    | CstI i -> Int i
    | CstB b -> Int (if b then 1 else 0)
    | CstS s -> String s
    | IfElse(e1, e2, e3) -> (match eval e1 with
        | Int 0 -> eval e3
        | Int _ -> eval e2
        | _ -> failwith "IfElse failed"
    )
    | Proj(Tuple(tuple, len), index) -> tuple_proj(Tuple(tuple, len), eval(index))
