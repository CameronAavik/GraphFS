open GraphFS.Graph
open GraphFS.Core.VertexSet
open GraphFS.Core.EdgeSet
open GraphFS.Algorithms.ShortestPath

let simpleExample () =
    // Persistent graphs!
    let g = 
        Graph.empty
        |> Graph.addEdge (0, 1)
        |> Graph.addEdges [(1, 2); (2, 3); (3, 4)]
        |> Graph.removeEdge (1, 2)

    // Interally, the graph is stored as a vertex set and an edge set, just like in your CS textbook

    printfn "Nodes: %A" (Graph.verts g |> Seq.toList) // [0; 1; 2; 3; 4]
    printfn "Neighbours of 2: %A" (Graph.neighbours 2 g |> Seq.toList) // [3]

let shortestPathExample () =
    let g = Graph.fromEdges [(0, 1); (1, 2); (2, 3); (3, 4); (4, 5); (5, 6); (6, 7); (3, -1); (-1, 7)]

    // this will find the shortest path from node 0 to 7
    let shortestPath = dijkstra 0 7 g

    printfn "Shortest path length from 0 to 7: %A" shortestPath // Some 5

type WeightInfo = { Weight: int }
let weightedShortestPathExample () =
    // this constructs a sequence of edges with a data type of WeightInfo
    let edgesWithData =
        [ (0, 1, 1); (1, 2, 2); (2, 3, 3); (3, 4, 4);
          (4, 5, 5); (5, 6, 6); (6, 7, 7); (3, -1, 4);
          (-1, 7, 99) ]
        |> Seq.map (fun (u, v, dist) -> ((u, v), { Weight = dist }))

    // graphs won't have edge data unless you make it explicit
    let g = Graph.fromEdgesWithData edgesWithData

    // weightedDijkstra will use a property called "Weight" on the edge data type
    let shortestPath = weightedDijkstra 0 7 g

    printfn "Shortest weighted path length from 0 to 7: %A" shortestPath // Some 28


let infiniteGridAstarExample () =
    // gets the manhattan distance between two points
    let manhattan p1 p2 =
        let (x1, y1), (x2, y2) = p1, p2
        let xDiff, yDiff = x2 - x1, y2 - y1
        abs xDiff + abs yDiff

    // we can also construct a graph by implementing the vertex set and edge set ourselves
        
    // assume all x, y coords are valid
    let vertexSet = { new IVertexSet<int * int> with member __.HasVert _ = true}

    // edges will connect two vertices if they have a manhattan distance of 1
    let edgeSet = 
        { new IEdgeSet<int * int> with
            member __.HasEdge edge =
                let p1, p2 = edge
                manhattan p1 p2 = 1
            member __.Neighbours p =
                let x, y = p
                upcast [| (x-1, y); (x+1, y); (x, y-1); (x, y+1) |] }
    
    // this graph is infinite!
    let infiniteGridGraph = Graph.fromSets vertexSet edgeSet

    // the manhattan distance is used as the heuristic
    let shortestPath = astar (0, 0) (1000, 1000) manhattan infiniteGridGraph

    printfn "Shortest path length from (0, 0) to (1000, 1000): %A" shortestPath // Some 2000
    ()


[<EntryPoint>]
let main argv =
    simpleExample()
    shortestPathExample()
    weightedShortestPathExample()
    infiniteGridAstarExample()
    0
