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

#load "PrettyPrint.fs"
open PrettyPrint
#load "TokenPrint.fsx"
open TokenPrint
#load "ProgramGrapher.fs"
open ProgramGrapher


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
        printfn "Result: Valid GCL program"

        // Pretty print ast
        printfn $"\nAST:\n{pretty_print e}\n"

        // Create program graph
        // Pass 'true' for deterministic program graph
        let z = create_program_graph e false

        // Save program graph to file
        write_graphviz z

    //
    with
    | _ -> printfn "Result: Invalid GCL program \n"



compute
