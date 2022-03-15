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



let node n = $"q{n} -> q{n + 1}"

let rec get_C e n =
    match e with
    | Assign (x, y) -> $"{node n} [label=\"{x}:={get_a y}\"]"
    | ArrAssign (x, y, z) -> $"{node n} [label=\"{x}[{get_a y}]:={get_a z}\"]"
    | Seq (x, y) -> $"{get_C x n}\n{get_C y (n + 1)}"
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

let create_program_graph ast = get_C ast 0

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
        let y = ast e
        let z = create_program_graph y
        printfn $"{z}"
        printfn $"{get_graphviz_link z}"
        // printfn $"Print:\n\n{pretty_print e}\n"
        printfn "Result: Valid GCL program"
    with
    | _ -> printfn "Result: Invalid GCL program \n"

compute
