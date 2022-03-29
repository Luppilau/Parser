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


// -----------------------------------------------------------------------------------------------------------------------------------------
// ---------------- HELPERS ---------------------

// ---------------- !HELPERS ---------------------

let rec eval_b e mem =
    match e with
    | Bool (x) -> x
    | SingleAnd (x, y) -> eval_b x mem && eval_b y mem // FIX
    | SingleOr (x, y) -> eval_b x mem || eval_b y mem // FIX
    | DoubleAnd (x, y) -> eval_b x mem && eval_b y mem
    | DoubleOr (x, y) -> eval_b x mem || eval_b y mem
    | NegB b -> not (eval_b b mem)
    | Eq (x, y) -> eval_a x mem = eval_a y mem
    | Neq (x, y) -> eval_a x mem <> eval_a y mem
    | Gt (x, y) -> eval_a x mem > eval_a y mem
    | Geq (x, y) -> eval_a x mem >= eval_a y mem
    | Lt (x, y) -> eval_a x mem < eval_a y mem
    | Leq (x, y) -> eval_a x mem <= eval_a y mem
    | ParB (x) -> eval_b x mem

and eval_a e mem =
    match e with
    | N (x) -> x
    | X (x) -> Map.find x mem
    | ArrRead (x, y) -> Map.find $"{x}[{eval_a y mem}]" mem
    | Add (x, y) -> eval_a x mem + eval_a y mem
    | Sub (x, y) -> eval_a x mem - eval_a y mem
    | Prod (x, y) -> eval_a x mem * eval_a y mem
    | Div (x, y) -> eval_a x mem / eval_a y mem
    | Neg (x) -> -eval_a x mem
    | Exp (x, y) -> pown (eval_a x mem) (eval_a y mem)
    | ParA (x) -> eval_a x mem


let get_expr_label expr =
    match expr with
    | Aexp (exp) -> get_a_label exp
    | Bexp (exp) -> get_b_label exp
    | Cexp (exp) -> get_c_label exp
    | _ -> failwith "Label failed"


let getNextNode n_s (pg: Edge list) memory =
    let matcher expr =
        match expr with
        | Bexp (x) -> eval_b x memory
        | Aexp (_) -> true
        | Cexp (_) -> true
        | _ -> failwith "Incorrect expression type"

    List.tryFind (fun (ns, _, expr) -> ns = n_s && matcher expr) pg

let execute_expr (memory: Map<string, int>) =
    function
    | Cexp (exp) ->
        match exp with
        | Assign (x, y) -> Map.add x (eval_a y memory) memory
        | ArrAssign (x, y, z) -> Map.add $"{x}[{eval_a y memory}]" (eval_a z memory) memory
        | _ -> failwith "Execution of expression doesn't make sense"
    | _ -> memory

let get_node_label node =
    match node with
    | Start -> "q\u25B7"
    | Step (Id (n)) -> "q" + string (n)
    | End -> "q\u25C0"

let interpret pg init_mem =
    let memory = Map.ofList init_mem
    printf "%-50s |" "Action"
    printf "%-8s |" "Node"
    Map.iter (fun v _ -> printf " %-8s |" v) memory
    printfn ""
    printf "%-50s |" ""
    printf "%-8s |" (get_node_label Start)
    Map.iter (fun _ v -> printf " %-8d |" v) memory
    printfn ""

    let rec interpret_recurse node pg (memory: Map<string, int>) =
        match node with
        | End -> "terminated"
        | _ ->
            match getNextNode node pg memory with
            | None -> "stuck"
            | Some ((n_s, n_e, expr)) ->
                let new_memory = execute_expr memory expr
                printf "%-50s |" (get_expr_label expr)
                printf "%-8s |" (get_node_label n_e)
                Map.iter (fun _ v -> printf " %-8d |" v) new_memory
                printfn ""
                interpret_recurse n_e pg new_memory

    let result = interpret_recurse Start pg memory
    printfn "\nstatus: %s\n" result



// -----------------------------------------------------------------------------------------------------------------------------------------



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

    let programPath =
        System.IO.Path.Combine(inputPath.FullName, "src/input-output/input.txt")

    let configInputPath =
        System.IO.Path.Combine(inputPath.FullName, "src/input-output/input.txt")

    let input = System.IO.File.ReadAllText programPath

    let configInput = System.IO.File.ReadAllText configInputPath

    //TODO extract configurations and save in memory map somehow

    // token_print input
    // try
    let e = parse input
    printfn "Result: Valid GCL program"

    // Pretty print ast
    printfn $"\nAST:\n{pretty_print e}\n"

    // Create program graph
    // Pass 'true' for deterministic program graph
    let z = create_program_graph e false
    write_graphviz z

    interpret
        z
        [ ("i", 0)
          ("n", 0)
          ("x", 0)
          ("y", 0)
          ("A[0]", 0)
          ("A[1]", 0)
          ("A[2]", 0) ]

    ()
// Save program graph to file

//
// with
// | _ -> printfn "Result: Invalid GCL program \n"



compute
