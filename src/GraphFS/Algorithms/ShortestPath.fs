namespace GraphFS.Algorithms

open GraphFS.Graph
open System.Collections.Generic

module ShortestPath =
    let inline private _dijkstra source target weightFunc graph =
        let seen = new HashSet<_>()

        let rec dijkstraInner fringe =
            if Set.isEmpty fringe then None
            else
                let dist, vertex = Set.minElement fringe
                let fringe' = Set.remove (dist, vertex) fringe
                if seen.Contains(vertex) then dijkstraInner fringe'
                elif vertex = target then Some dist
                else
                    seen.Add vertex |> ignore
                    Graph.neighbours vertex graph
                    |> Seq.map (fun v -> (dist + weightFunc (vertex, v), v))
                    |> Set.ofSeq
                    |> Set.union fringe'
                    |> dijkstraInner

        set [(0, source)] |> dijkstraInner

    let inline dijkstra source target graph = _dijkstra source target (fun _ -> 1) graph
    let inline weightedDijkstra source target graph =
        let inline getWeight edge =
            let edgeData = Graph.getEdgeData edge graph
            (^E : (member Weight : int) edgeData)
        _dijkstra source target getWeight graph

    let private _astar source target heuristic weightFunc graph =
        let seen = new HashSet<_>()
        let h v = heuristic v target

        let rec astarInner fringe =
            if Set.isEmpty fringe then None
            else
                let (_, negDist, vertex) as minElem = Set.minElement fringe
                let fringe' = Set.remove minElem fringe
                let dist = -negDist
                if seen.Contains(vertex) then astarInner fringe'
                elif vertex = target then Some dist
                else
                    seen.Add vertex |> ignore
                    Graph.neighbours vertex graph
                    |> Seq.map (fun v -> (dist + weightFunc (vertex, v), v))
                    |> Seq.map (fun (dist, v) -> ((h v) + dist, -dist, v))
                    |> Set.ofSeq
                    |> Set.union fringe'
                    |> astarInner
                    
        set [(h source, 0, source)] |> astarInner

    let astar source target heuristic graph = _astar source target heuristic (fun _ -> 1) graph
    //let inline weightedAstar source target heuristic graph =
    //    let inline getWeight edge =
    //        let edgeData = Graph.getEdgeData edge graph
    //        (^E : (member Weight : int) edgeData)
    //    _astar source target heuristic getWeight graph
        
