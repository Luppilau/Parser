module ProgramGrapher

open FM4FUNTypesAST

let get_graphviz_link program_graph =
    let base_url = "https://dreampuf.github.io/GraphvizOnline/#digraph%20G"

    let result = $"{base_url}%%7B%%0A{program_graph}%%0A%%7D"

    result.Replace("\n", "%0A")
