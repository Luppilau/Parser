module PrettyPrint

open FM4FUNTypesAST

let rec pretty_print e =
    match e with
    | Assign (x, y) -> $"Assign({x},{eval_a y})"
    | ArrAssign (x, y, z) -> $"ArrAssign({x},{eval_a y},{eval_a z})"
    | Seq (x, y) -> $"Seq({pretty_print x},{pretty_print y})"
    | If (x) -> $"If({eval_GC x})"
    | Do (x) -> $"Do({eval_GC x})"
    | Skip -> "Skip()"

and eval_GC e =
    match e with
    | Cond (x, y) -> $"Cond({eval_b x},{pretty_print y})"
    | Conc (x, y) -> $"Conc({eval_GC x},{eval_GC y})"

and eval_b e =
    match e with
    | Bool (x) -> $"Bool({x})"
    | SingleAnd (x, y) -> $"SingleAnd({eval_b x},{eval_b y})"
    | SingleOr (x, y) -> $"SingleOr({eval_b x},{eval_b y})"
    | DoubleAnd (x, y) -> $"DoubleAnd({eval_b x},{eval_b y})"
    | DoubleOr (x, y) -> $"DoubleOr({eval_b x},{eval_b y})"
    | NegB x -> $"NegB({eval_b x})"
    | Eq (x, y) -> $"Eq({eval_a x},{eval_a y})"
    | Neq (x, y) -> $"Neq({eval_a x},{eval_a y})"
    | Gt (x, y) -> $"Gt({eval_a x},{eval_a y})"
    | Geq (x, y) -> $"Geq({eval_a x},{eval_a y})"
    | Lt (x, y) -> $"Lt({eval_a x},{eval_a y})"
    | Leq (x, y) -> $"Leq({eval_a x},{eval_a y})"

and eval_a e =
    match e with
    | N (x) -> $"N({x})"
    | X (x) -> $"X({x})"
    | ArrRead (x, y) -> $"ArrRead({x},{eval_a y})"
    | Add (x, y) -> $"Add({eval_a x},{eval_a y})"
    | Sub (x, y) -> $"Sub({eval_a x},{eval_a y})"
    | Prod (x, y) -> $"Prod({eval_a x},{eval_a y})"
    | Div (x, y) -> $"Div({eval_a x},{eval_a y})"
    | Neg (x) -> $"Neg({eval_a x})"
    | Exp (x, y) -> $"Exp({eval_a x},{eval_a y})"
