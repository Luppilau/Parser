module ProgramGrapher

open FM4FUNTypesAST


type Id = Id of int

type Node =
    | Start
    | Step of Id
    | End

type Expression =
    | Aexp of a
    | Bexp of b
    | Cexp of C
    | GCexp of GC

type Edge = Node * Node * Expression

// ---------------- HELPERS ---------------------

let mutable id_int = 0
let mutable is_deterministic = false

let get_id =
    fun () ->
        id_int <- id_int + 1
        id_int |> Id

let rec get_det exp =
    match exp with
    | Cond (x, _) -> x
    | Conc (x, y) -> SingleAnd(get_det x, get_det y)

let rec get_do_b x exp =
    match is_deterministic with
    | true ->
        match x with
        | Cond (x, _) -> SingleOr(x, exp)
        | Conc (x, y) -> ParB(get_do_b y (ParB((SingleOr(get_do_b x exp, exp)))))
    | false ->
        match x with
        | Cond (x, _) -> ParB(NegB(ParB(x)))
        | Conc (x, y) -> SingleAnd(get_do_b x exp, get_do_b y exp)

// ---------------- !HELPERS ---------------------

let rec get_C e (n_s: Node) (n_e: Node) =
    match e with
    | Assign (_, _) -> [ Edge(n_s, n_e, (Cexp(e))) ]
    | ArrAssign (_, _, _) -> [ Edge(n_s, n_e, Cexp(e)) ]
    | Seq (x, y) ->
        let n_new = (Step(get_id ()))
        (get_C x n_s n_new) @ (get_C y n_new n_e)

    | If (x) -> get_GC x n_s n_e (Bool(false))
    | Do (x) ->
        match is_deterministic with
        | true ->
            //    let maxEdge = fst get_GC x n_s n_s (Bool(false)) |>
            //        List.fold (fun (_,b,_), acc -> SingleOr(b,acc)) Bool(true)
            Edge(n_s, n_e, (Bexp(NegB(ParB(edge_do_det x)))))
            :: (get_GC x n_s n_s (Bool(false)))
        | false ->
            Edge(n_s, n_e, (Bexp(edge_do_nondet x)))
            :: (get_GC x n_s n_s (Bool(false)))
    | Skip -> [ Edge(n_s, n_e, Cexp(Skip)) ]

and edge_do_det x =
    match x with
    | Cond (x, _) -> x
    | Conc (x, y) -> SingleOr(ParB(edge_do_det x), ParB(edge_do_det y))

and edge_do_nondet x =
    match x with
    | Cond (x, _) -> NegB(ParB(x))
    | Conc (x, y) -> SingleAnd(ParB(edge_do_nondet x), ParB(edge_do_nondet y))

and get_GC e n_s n_e acc =
    match e with
    | Cond (x, y) ->
        let n_new = (Step(get_id ()))

        match is_deterministic with
        | true ->
            (Edge(n_s, n_new, Bexp(SingleAnd(ParB(x), ParB(NegB(acc))))))
            :: (get_C y n_new n_e)
        | false -> (Edge(n_s, n_new, Bexp(x)) :: (get_C y n_new n_e))

    | Conc (x, y) ->
        match is_deterministic with
        | true ->
            (get_GC x n_s n_e acc)
            @ (get_GC y n_s n_e (edge_conc x acc))
        | false ->
            (get_GC x n_s n_e (Bool(false)))
            @ (get_GC y n_s n_e (Bool(false)))

and edge_conc x acc =
    ParB(SingleOr(ParB(recurse_edge_conc x), (acc)))

and recurse_edge_conc exp =
    match exp with
    | Cond (x, _) -> x
    | Conc (x, y) -> SingleOr(recurse_edge_conc x, recurse_edge_conc y)


let rec get_conc_edge_type e =
    match e with
    | _ -> e

// ----------------------- LABEL HELPERS ----------------------
let rec get_gc_label e =
    match e with
    | Cond x -> $"{x}"
    | Conc x -> $"{x}"

let rec get_c_label e =
    match e with
    | Assign (x, y) -> $"{x}:={get_a_label y}"
    | ArrAssign (x, y, z) -> $"{x}[{get_a_label y}]:={get_a_label z}"
    | _ -> ""

and get_b_label e =
    match e with
    | Bool (x) -> $"{x}"
    | SingleAnd (x, y) -> $"{get_b_label x} & {get_b_label y}"
    | SingleOr (x, y) -> $"{get_b_label x} | {get_b_label y}"
    | DoubleAnd (x, y) -> $"{get_b_label x} && {get_b_label y}"
    | DoubleOr (x, y) -> $"{get_b_label x} || {get_b_label y}"
    | NegB x -> $"!{get_b_label x}"
    | Eq (x, y) -> $"{get_a_label x} = {get_a_label y}"
    | Neq (x, y) -> $"{get_a_label x} != {get_a_label y}"
    | Gt (x, y) -> $"{get_a_label x} > {get_a_label y}"
    | Geq (x, y) -> $"{get_a_label x} >= {get_a_label y}"
    | Lt (x, y) -> $"{get_a_label x} < {get_a_label y}"
    | Leq (x, y) -> $"{get_a_label x} <= {get_a_label y}"
    | ParB (x) -> $"({get_b_label x})"

and get_a_label e =
    match e with
    | N (x) -> $"{x}"
    | X (x) -> x
    | ArrRead (x, y) -> $"{x}[{get_a_label y}]"
    | Add (x, y) -> $"{get_a_label x}+{get_a_label y}"
    | Sub (x, y) -> $"{get_a_label x}-{get_a_label y}"
    | Prod (x, y) -> $"{get_a_label x}*{get_a_label y}"
    | Div (x, y) -> $"{get_a_label x}/{get_a_label y}"
    | Neg (x) -> $"-{get_a_label x}"
    | Exp (x, y) -> $"{get_a_label x}^{get_a_label y}"
    | ParA (x) -> $"({get_a_label x})"

// ----------------------- !LABEL HELPERS ----------------------


let create_program_graph ast det =
    is_deterministic <- det
    get_C ast Start End


let write_graphviz program_graph =
    let rec format program_graph =
        match program_graph with
        | x :: tail -> convert x + format tail
        | _ -> ""

    and convert edge =
        match edge with
        | (n_1, n_2, Aexp (exp)) -> $"q{get_node_id n_1} -> q{get_node_id n_2} [label=\"{get_a_label exp}\"] \n"
        | (n_1, n_2, Bexp (exp)) -> $"q{get_node_id n_1} -> q{get_node_id n_2} [label=\"{get_b_label exp}\"] \n"
        | (n_1, n_2, Cexp (exp)) -> $"q{get_node_id n_1} -> q{get_node_id n_2} [label=\"{get_c_label exp}\"] \n"
        | (n_1, n_2, GCexp (exp)) -> $"q{get_node_id n_1} -> q{get_node_id n_2} [label=\"{get_gc_label exp}\"] \n"

    and get_node_id node =
        match node with
        | Start -> "\u25B7"
        | Step (Id (n)) -> string (n)
        | End -> "\u25C0"

    let wrap_graph graph =
        "digraph G {\nrankdir=LR;\nnode [shape = circle]\n"
        + graph
        + "}"

    System.IO.File.WriteAllText("input-output/output.dot", wrap_graph (format program_graph))
