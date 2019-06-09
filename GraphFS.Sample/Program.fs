open GraphFS.Graph

[<EntryPoint>]
let main argv =
    let g = 
        Graph.empty
        |> Graph.addEdge (0, 1)
        |> Graph.addEdges [(1, 2); (2, 3); (3, 4)]
        |> Graph.removeEdge (1, 2)

    printfn "Nodes: %A" (Graph.verts g |> Seq.toList) // Nodes: [0; 1; 2; 3; 4]
    printfn "Neighbours of 2: %A" (Graph.neighbours 2 g |> Seq.toList) // Neighbours of 2: [3]

    0
