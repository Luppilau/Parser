// This script implements our interactive FM4FUN

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "FM4FUNTypesAST.fs"
open FM4FUNTypesAST
#load "FM4FUNParser.fs"
open FM4FUNParser
#load "FM4FUNLexer.fs"
open FM4FUNLexer

#load "SyntaxTree.fs"
open SyntaxTree
#load "PrettyPrint.fs"
open PrettyPrint
#load "TokenPrint.fsx"
open TokenPrint
#load "ProgramGrapher.fs"
open ProgramGrapher



type Node =
    | Start
    | Intermediate of int
    | End

type Edge = Node * Node * string

let rec get_C e acc (n_s: Node) (n_e: Node) =
    match e with
    | Assign (x, y) -> ([ Edge(n_s, n_e, $"{x}:={get_a y}") ], acc + 1)
    | ArrAssign (x, y, z) -> ([ Edge(n_s, n_e, $"{x}[{get_a y}]:={get_a z}") ], acc + 1)
    | Seq (x, y) ->
        let (arr, ac) = (get_C x acc n_s (Intermediate(acc)))
        let (arr2, ac2) = get_C y ac (Intermediate(acc)) n_e
        ((arr @ arr2), ac2 + 1)
    | If (x) -> get_GC x acc n_s n_e
    | Do (x) ->
        ((Edge(n_s, n_e, $"!{get_b (get_do_b x)}")
          :: fst (get_GC x (acc) n_s n_s)),
         acc)
    //  | Skip -> Skip
    | _ -> ([ Edge(n_s, n_e, "_") ], acc)

and get_do_b x =
    match x with
    | Cond (x, _) -> x
    | Conc (x, y) -> SingleAnd(get_do_b x, get_do_b y)

and get_GC e acc n_s n_e =
    match e with
    | Cond (x, y) ->
        let (arr, ac) = get_C y (acc + 1) (Intermediate(acc)) n_e
        ((Edge(n_s, Intermediate(acc), $"{get_b x}") :: arr), ac)

    | Conc (x, y) ->
        let (arr, ac) = (get_GC x acc n_s n_e)
        printfn $"{ac}"
        let (arr2, ac2) = (get_GC y ac n_s n_e)
        printfn $"{ac2}"
        ((arr @ arr2), ac2)

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

let create_program_graph ast = fst (get_C ast 1 Start End)


let get_graphviz_link program_graph =

    let rec format program_graph n =
        match program_graph with
        | x :: tail -> convert x n + format tail (n + 1)
        | _ -> ""

    and convert edge n =
        match edge with
        | (n_1, n_2, expr) -> $"q{get_node_id n_1 n} -> q{get_node_id n_2 (n + 1)} [label=\"{expr}\"] \n"

    and get_node_id node n =
        match node with
        | Start -> "s"
        | Intermediate (n) -> string (n)
        | End -> "e"

    let base_url = "https://dreampuf.github.io/GraphvizOnline/#digraph%20G"

    let result = $"{base_url}%%7B%%0A{format program_graph 0}%%0A%%7D"
    let final = result.Replace("\n", "%0A")

    $"\n{final}\n"

// -------------------------------------------------------------------------------- //

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = FM4FUNParser.start FM4FUNLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res


// We implement here the function that interacts with the user
let rec compute =
    let inputPath = System.IO.Directory.GetParent(__SOURCE_DIRECTORY__)
    let fullPath = System.IO.Path.Combine(inputPath.FullName, "src/input.txt")
    let input = System.IO.File.ReadAllText fullPath

    // token_print input

    try
        let e = parse input
        printfn $"Print:\n\n{pretty_print e}\n"
        let y = ast e
        let z = create_program_graph y
        printfn $"{z}"
        printfn $"{get_graphviz_link z}"
        printfn "Result: Valid GCL program"
    with
    | _ -> printfn "Result: Invalid GCL program \n"

compute
