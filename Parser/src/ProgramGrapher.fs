module ProgramGrapher

open FM4FUNTypesAST

type Id = Id of int

type Node =
    | Start
    | Step of Id
    | End

type Edge = Node * Node * string

let get_id =
    let mutable id_int = 0

    fun () ->
        id_int <- id_int + 1
        id_int |> Id

let rec get_C e id (n_s: Node) (n_e: Node) =
    match e with
    | Assign (x, y) -> [ Edge(n_s, n_e, $"{x}:={get_a y}") ]
    | ArrAssign (x, y, z) -> [ Edge(n_s, n_e, $"{x}[{get_a y}]:={get_a z}") ]
    | Seq (x, y) ->
        let new_node = (Step(id ()))

        (get_C x id n_s new_node)
        @ (get_C y id new_node n_e)

    | If (x) -> get_GC x id n_s n_e
    | Do (x) ->
        get_GC x id n_s n_s
        @ [ Edge(n_s, n_e, $"!{get_b (get_do_b x)}") ]
    | Skip -> [ Edge(n_s, n_e, "skip") ]

and get_do_b x =
    match x with
    | Cond (x, _) -> x
    | Conc (x, y) -> SingleAnd(get_do_b x, get_do_b y)

and get_GC e id n_s n_e =
    match e with
    | Cond (x, y) ->
        let new_node = (Step(id ()))

        (get_C y id new_node n_e)
        @ [ Edge(n_s, new_node, $"{get_b x}") ]

    | Conc (x, y) -> (get_GC x id n_s n_e) @ (get_GC y id n_s n_e)

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
    | ParB (x) -> $"({get_b x})"


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
    | ParA (x) -> $"({get_a x})"


let create_program_graph ast = get_C ast get_id Start End


let write_graphviz program_graph =
    let rec format program_graph n =
        match program_graph with
        | x :: tail -> convert x n + format tail (n + 1)
        | _ -> ""

    and convert edge n =
        match edge with
        | (n_1, n_2, expr) -> $"q{get_node_id n_1 n} -> q{get_node_id n_2 (n + 1)} [label=\"{expr}\"] \n"

    and get_node_id node n =
        match node with
        | Start -> "\u25B7"
        | Step (Id (n)) -> string (n)
        | End -> "\u25C0"

    let wrap_graph graph =
        "digraph G {\nrankdir=LR;\nnode [shape = circle]\n"
        + graph
        + "}"

    System.IO.File.WriteAllText("input-output/output.dot", wrap_graph (format program_graph 0))
