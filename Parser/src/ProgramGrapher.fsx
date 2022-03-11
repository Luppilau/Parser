module ProgramGrapher

#load "FM4FUNTypesAST.fs"
open FM4FUNTypesAST

// https://dreampuf.github.io/GraphvizOnline/#digraph%20G%20%7B%0A%20%20a1%20-%3E%20b3%20%5Blabel%3Dtest%5D%3B%0A%20%20b2%20-%3E%20a3%3B%0A%20%20a3%20-%3E%20a0%3B%0A%20%20a3%20-%3E%20end%3B%0A%20%20b3%20-%3E%20end%3B%0A%7D%0A

let get_graphviz_link program_graph =
    let base_url = "https://dreampuf.github.io/GraphvizOnline/#digraph"
    base_url

let rec create_program_graph ast =
    match ast with
    | Assign (x, y) -> "goodbye"
    | _ -> "hello"




// type BinaryTree<'a> =
//     |Leaf
//     |Node of BinaryTree<'a> * 'a * BinaryTree<'a>

// let rec preorder (tr:BinaryTree<'a>) : 'a list =
//     match tr with
//     |Leaf            -> []
//     |Node(trr,x,trl) -> x:: (preorder trr) @ (preorder trl)

// let rec inorder (tree:BinaryTree<'a>) : 'a list =
//     match tree with
//     |Leaf -> []
//     |Node(tr,x,tl) -> (inorder tr) @ [x] @ (inorder tl)

// let rec postorder (tree:BinaryTree<'a>) : 'a list =
//     match tree with
//     |Leaf -> []
//     |Node(tr,x,tl) -> (postorder tr) @ (postorder tl) @ [x]

// let someTree = Node(Leaf,20,Node(Leaf,40,Node(Node(Leaf,-2,Leaf),2,Node(Leaf,0,Leaf))))

// preorder someTree
// inorder someTree
// postorder someTree
