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

let rec eval_C e =
    match e with
    | Assign (x, y) -> $"Assign({x},{eval_a y})" //
    | ArrAssign (x, y, z) -> $"ArrAssign({x},{eval_a y},{eval_a z})" //
    | Seq (x, y) -> $"Seq({eval_C x},{eval_C y})" //
    | If (x) -> $"If({eval_GC x})" //
    | Do (x) -> $"Do({eval_GC x})" //
    | Skip -> "Skip()" //

and eval_GC e =
    match e with
    | Cond (x, y) -> $"Cond({eval_b x},{eval_C y})" //
    | Conc (x, y) -> $"Conc({eval_GC x},{eval_GC y})" // kun med BOX

and eval_b e =
    match e with
    | Bool (x) -> $"Bool({x})"
    | SingleAnd (x, y) -> $"SingleAnd({eval_b x},{eval_b y})" //
    | SingleOr (x, y) -> $"SingleOr({eval_b x},{eval_b y})" //
    | DoubleAnd (x, y) -> $"DoubleAnd({eval_b x},{eval_b y})" //
    | DoubleOr (x, y) -> $"DoubleOr({eval_b x},{eval_b y})" //
    | NegB x -> $"NegB({eval_b x})" //
    | Eq (x, y) -> $"Eq({eval_a x},{eval_a y})" //
    | Neq (x, y) -> $"Neq({eval_a x},{eval_a y})" //
    | Gt (x, y) -> $"Gt({eval_a x},{eval_a y})" //
    | Geq (x, y) -> $"Geq({eval_a x},{eval_a y})" //
    | Lt (x, y) -> $"Lt({eval_a x},{eval_a y})" //
    | Leq (x, y) -> $"Leq({eval_a x},{eval_a y})" //

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

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = FM4FUNParser.start FM4FUNLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let tokenPrinter input =
    let lexbuf = LexBuffer<char>.FromString input

    let allTokens =
        Seq.initInfinite (fun _ -> FM4FUNLexer.tokenize lexbuf)
        |> Seq.takeWhile ((<>) token.EOF)

    let rec allTokens e =
        match e with
        | token.EOF -> []
        | t -> t :: allTokens (FM4FUNLexer.tokenize lexbuf)

    let x = allTokens (FM4FUNLexer.tokenize lexbuf)

    let printList list =
        let rec helper index list =
            match list with
            | [] -> ()
            | head :: tail ->
                printfn "%d: %A" index head
                helper (index + 1) tail

        helper 1 list

    printList x

// We implement here the function that interacts with the user
let rec compute =
    let inputPath = System.IO.Directory.GetParent(__SOURCE_DIRECTORY__)
    let fullPath = System.IO.Path.Combine(inputPath.FullName, "src/input.txt")
    let input = System.IO.File.ReadAllText fullPath

    // tokenPrinter input
    printfn "Input:\n\n%s\n" input

    printfn "--------------"

    try
        let e = parse input
        printfn $"Result:\n\n{eval_C e}\n"
    with
    | _ -> printfn "Result:\n\nInvalid input \n"


compute
