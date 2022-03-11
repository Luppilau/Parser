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

#load "SyntaxTree.fsx"
open SyntaxTree
#load "PrettyPrint.fsx"
open PrettyPrint
#load "TokenPrint.fsx"
open TokenPrint
#load "ProgramGrapher.fsx"
open ProgramGrapher

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
    let x = parse input
    let y = ast x
    let z = create_program_graph y

    try
        let e = parse input
        printfn "Result: Valid GCL program"
        printfn $"Print:\n\n{pretty_print e}\n"
    with
    | _ -> printfn "Result: Invalid GCL program \n"


compute
