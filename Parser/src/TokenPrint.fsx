module TokenPrint

#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing

#load "FM4FUNParser.fs"
#load "FM4FUNLexer.fs"

let token_print input =
    let lexbuf = LexBuffer<char>.FromString input

    let allTokens =
        Seq.initInfinite (fun _ -> FM4FUNLexer.tokenize lexbuf)
        |> Seq.takeWhile ((<>) FM4FUNParser.token.EOF)

    let rec allTokens e =
        match e with
        | FM4FUNParser.token.EOF -> []
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
