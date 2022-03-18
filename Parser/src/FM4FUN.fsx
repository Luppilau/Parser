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

    let fullPath =
        System.IO.Path.Combine(inputPath.FullName, "src/input-output/input.txt")

    let input = System.IO.File.ReadAllText fullPath

    // token_print input
    try
        let e = parse input
        let y = ast e
        let z = create_program_graph y false
        printfn $"\n{z}\n"
        write_graphviz z
        printfn "Result: Valid GCL program"
        printfn $"Print:\n\n{pretty_print e}\n"
    with
    | _ -> printfn "Result: Invalid GCL program \n"



compute
