module ProgramGrapher

open FM4FUNTypesAST

let get_graphviz_link program_graph =
    let base_url = "https://dreampuf.github.io/GraphvizOnline/#digraph%20G"

    let result = $"{base_url}%%7B%%0A{program_graph}%%0A%%7D"

    result.Replace("\n", "%0A")

let node n = $"q{n} -> q{n + 1}"

let rec traverse e n =
    match e with
    | Assign (x, y) -> $"{node n} [label=\"{x}:={get_a y}\"]"
    | ArrAssign (x, y, z) -> $"{node n} [label=\"{x}[{get_a y}]:={get_a z}\"]"
    | Seq (x, y) -> $"{traverse x n}\n{traverse y (n + 1)}"
    // | If (x) -> If(eval_GC x)
// | Do (x) -> Do(eval_GC x)
// | Skip -> Skip
    | _ -> ""

// and eval_GC e =
//     match e with
//     | Cond (x, y) -> Cond(eval_b x, ast y)
//     | Conc (x, y) -> Conc(eval_GC x, eval_GC y)

and get_b e =
    match e with
    | Bool (x) -> $"{x}"
    | SingleAnd (x, y) -> $"{get_b x} & {get_b y}"
    | SingleOr (x, y) -> $"{get_b x} | {get_b y}"
    | DoubleAnd (x, y) -> $"{get_b x} && {get_b y}"
    | DoubleOr (x, y) -> $"{get_b x} || {get_b y}"
    | NegB x -> $"!{get_b x}"
    | Eq (x, y) -> $"{get_a x} = {get_a y}"
    | Neq (x, y) -> $"{get_a x} != {get_a y}"
    | Gt (x, y) -> $"{get_a x} > {get_a y}"
    | Geq (x, y) -> $"{get_a x} >= {get_a y}"
    | Lt (x, y) -> $"{get_a x} < {get_a y}"
    | Leq (x, y) -> $"{get_a x} <= {get_a y}"

and get_a e =
    match e with
    | N (x) -> $"{x}"
    | X (x) -> x
    | ArrRead (x, y) -> $"{x}[{get_a y}]"
    | Add (x, y) -> $"{get_a x}+{get_a y}"
    | Sub (x, y) -> $"{get_a x}-{get_a y}"
    | Prod (x, y) -> $"{get_a x}*{get_a y}"
    | Div (x, y) -> $"{get_a x}/{get_a y}"
    | Neg (x) -> $"-{get_a x}"
    | Exp (x, y) -> $"{get_a x}^{get_a y}"

let create_program_graph ast = traverse ast 0
