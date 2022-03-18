module Interpreter

open FM4FUNTypesAST



// ---------------- HELPERS ---------------------

let variables = Set.empty
let arrays = Set.empty

// ---------------- !HELPERS ---------------------

let rec eval_b e =
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
    | ParB (x) -> eval_b x

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
    | ParA (x) -> eval_a x
