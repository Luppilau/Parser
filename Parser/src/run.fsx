#load "FM4FUNTypesAST.fs"
open FM4FUNTypesAST
#load "FM4FUNParser.fs"
open FM4FUNParser
#load "FM4FUNLexer.fs"
open FM4FUNLexer
#load "ProgramGrapher.fs"
open ProgramGrapher

let getAction (memory: Map<string, a>) =
    function
    | (ns, ne, expr) ->
        match expr with
        | Assign (x, y) -> memory.Add(x, y)
        | _ -> memory.Add("t", N(0)) //placeholder
