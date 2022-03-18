module ProgramGrapher

open FM4FUNTypesAST


type Id = Id of int

type Node =
    | Start
    | Step of Id
    | End

type Edge = Node * Node * string

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

let rec get_do_b x =
    match x with
    | Cond (x, _) -> x
    | Conc (x, y) -> SingleAnd(get_do_b x, get_do_b y)

let edge_label_conc exp x =
    if is_deterministic then
        (SingleAnd(get_det x, ParB(NegB(exp))))
    else
        exp

let edge_label_cond exp x =
    if is_deterministic then
        (SingleAnd(x, ParB(NegB(exp))))
    else
        x

// ---------------- HELPERS ---------------------

let rec get_C e (n_s: Node) (n_e: Node) =
    match e with
    | Assign (x, y) -> [ Edge(n_s, n_e, $"{x}:={get_a y}") ]
    | ArrAssign (x, y, z) -> [ Edge(n_s, n_e, $"{x}[{get_a y}]:={get_a z}") ]
    | Seq (x, y) ->
        let n_new = (Step(get_id ()))
        (get_C x n_s n_new) @ (get_C y n_new n_e)

    | If (x) -> get_GC x n_s n_e (Bool(false))
    | Do (x) ->
        get_GC x n_s n_s (Bool(false))
        @ [ Edge(n_s, n_e, $"!{get_b (get_do_b x)}") ]
    | Skip -> [ Edge(n_s, n_e, "skip") ]

and get_GC e n_s n_e exp =
    match e with
    | Cond (x, y) ->
        let n_new = (Step(get_id ()))

        (get_C y n_new n_e)
        @ [ Edge(n_s, n_new, $"{get_b (edge_label_cond exp x)}") ]

    | Conc (x, y) ->
        (get_GC x n_s n_e exp)
        @ (get_GC y n_s n_e (edge_label_conc exp x))

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


let create_program_graph ast det =
    is_deterministic <- det
    get_C ast Start End
