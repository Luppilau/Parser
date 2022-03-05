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

let variables = Map []
let arrays = Map []

let rec eval_C e =
    match e with
    | Assign (x, y) -> ignore (Map.add x (eval_a y) variables)
    | ArrAssign (x, y, z) -> array_assign x y z
    | Seq (x, y) ->
        eval_C (x)
        eval_C (y)
    | If (x) -> eval_GC (x)
    | Do (x) -> eval_GC (x)
    | Skip -> ()

and eval_GC e =
    match e with
    | Cond (x, y) -> if eval_b (x) then eval_C (y)
    | Conc (x, y) ->
        eval_GC (x)
        eval_GC (y)

and eval_b e =
    match e with
    | Bool (x) -> x
    | SingleAnd (x, y) -> eval_b (x) && eval_b (y) // FIX
    | SingleOr (x, y) -> eval_b (x) || eval_b (y) // FIX
    | DoubleAnd (x, y) -> eval_b (x) && eval_b (y)
    | DoubleOr (x, y) -> eval_b (x) || eval_b (y)
    | NegB b -> not (eval_b (b))
    | Eq (x, y) -> eval_a (x) = eval_a (y)
    | Neq (x, y) -> eval_a (x) <> eval_a (y)
    | Gt (x, y) -> eval_a (x) > eval_a (y)
    | Geq (x, y) -> eval_a (x) >= eval_a (y)
    | Lt (x, y) -> eval_a (x) < eval_a (y)
    | Leq (x, y) -> eval_a (x) <= eval_a (y)

and eval_a e =
    match e with
    | N (x) -> x
    | X (x) -> Map.find x variables
    | ArrRead (x, y) -> Map.find (eval_a (y)) (Map.find x arrays)
    | Add (x, y) -> eval_a (x) + eval_a (y)
    | Sub (x, y) -> eval_a (x) - eval_a (y)
    | Prod (x, y) -> eval_a (x) * eval_a (y)
    | Div (x, y) -> eval_a (x) / eval_a (y)
    | Neg (x) -> - eval_a(x)
    | Exp (x, y) -> pown (eval_a (x)) (eval_a (y))

and array_assign x y z =
    if Map.containsKey x arrays then
        ignore (Map.add (eval_a y) (eval_a z) (Map.find x arrays))
    else
        ignore (Map.add x (Map.add (eval_a y) (eval_a z) Map.empty) arrays)



// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    printfn ""
    printfn "Lexbuf success"
    let tokens = FM4FUNLexer.tokenize lexbuf

    let res = FM4FUNParser.start tokens
    // return the result of parsing (i.e. value of type "expr")
    printfn "Result success"
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        try
            // let e = parse (Console.ReadLine())
            let e = parse "1+2"
            eval_C (e)
            printfn "eval success"
            printfn "Result: %A" variables
            printfn "Result: %A" arrays
            compute n
        with
        | err -> compute (n - 1)

// Start interacting with the user
compute 3
