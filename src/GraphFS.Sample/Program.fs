open GraphFS.Graph
open GraphFS.Core.VertexSet
open GraphFS.Core.EdgeSet

let simpleExample () =
    let g = 
        Graph.empty
        |> Graph.addEdge (0, 1)
        |> Graph.addEdges [(1, 2); (2, 3); (3, 4)]
        |> Graph.removeEdge (1, 2)

    printfn "Nodes: %A" (Graph.verts g |> Seq.toList) // Nodes: [0; 1; 2; 3; 4]
    printfn "Neighbours of 2: %A" (Graph.neighbours 2 g |> Seq.toList) // Neighbours of 2: [3]
    

let infiniteGridExample () =
    let vertexSet = { new IVertexSet<int * int> with member __.HasVert _ = true}
    let edgeSet = 
        { new IEdgeSet<int * int> with
            member __.HasEdge edge =
                let (x1, y1), (x2, y2) = edge
                let xDiff, yDiff = abs (x2 - x1), abs (y2 - y1)
                (xDiff = 1 && yDiff = 0) || (xDiff = 0 && yDiff = 1)
            member __.Neighbours p =
                let x, y = p
                upcast [| (x-1, y); (x+1, y); (x, y-1); (x, y+1) |] }
    let gridGraph = Graph.fromSets vertexSet edgeSet
    ()


[<EntryPoint>]
let main argv =
    simpleExample()
    infiniteGridExample()
    0
