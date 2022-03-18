module Interpreter

open FM4FUNTypesAST

// ---------------- HELPERS ---------------------


// ---------------- !HELPERS ---------------------


let rec eval_C e =
    match e with
    | Assign (x, y) -> () // FIX
    | ArrAssign (x, y, z) -> () // FIX
    | Seq (x, y) ->
        eval_C (x)
        eval_C (y)
    | If (x) -> eval_GC (x)
    | Do (x) -> eval_GC (x)
    | Skip -> ()

and eval_GC e =
    match e with
    | Cond (x, y) -> if eval_b (x) then eval_C (y)
    | Conc (x, y) ->
        eval_GC (x)
        eval_GC (y)

and eval_b e =
    match e with
    | Bool (x) -> x
    | SingleAnd (x, y) -> eval_b (x) && eval_b (y) // FIX
    | SingleOr (x, y) -> eval_b (x) || eval_b (y) // FIX
    | DoubleAnd (x, y) -> eval_b (x) && eval_b (y)
    | DoubleOr (x, y) -> eval_b (x) || eval_b (y)
    | NegB b -> not (eval_b (b))
    | Eq (x, y) -> eval_a (x) = eval_a (y)
    | Neq (x, y) -> eval_a (x) <> eval_a (y)
    | Gt (x, y) -> eval_a (x) > eval_a (y)
    | Geq (x, y) -> eval_a (x) >= eval_a (y)
    | Lt (x, y) -> eval_a (x) < eval_a (y)
    | Leq (x, y) -> eval_a (x) <= eval_a (y)

and eval_a e =
    match e with
    | N (x) -> x
    | X (x) -> 0 // FIX
    | ArrRead (x, y) -> 0 // FIX
    | Add (x, y) -> eval_a (x) + eval_a (y)
    | Sub (x, y) -> eval_a (x) - eval_a (y)
    | Prod (x, y) -> eval_a (x) * eval_a (y)
    | Div (x, y) -> eval_a (x) / eval_a (y)
    | Neg (x) -> - eval_a(x)
    | Exp (x, y) -> pown (eval_a (x)) (eval_a (y))
