namespace GraphFS.Core

open System.Collections.Generic

module Edge =
    let inline edgeToVerts (u, v) = [|u; v|]
    let inline edgeWithDataToVerts ((u, v), _) = [|u; v|]
    let inline multiEdgeToVerts (u, v, _) = [|u; v|]
    let inline multiEdgeWithDataToVerts ((u, v, _), _) = [|u; v|]

    let inline vertsFromEdgeSeq (toVerts : 'a -> 'b array) edges =
        let vertSet = new HashSet<'b>()
        for edge in edges do
            let vertArr = toVerts edge
            vertSet.Add vertArr.[0] |> ignore
            vertSet.Add vertArr.[1] |> ignore
        vertSet |> Seq.toArray

    let inline edgesToVerts edges =  vertsFromEdgeSeq edgeToVerts edges
    let inline edgesWithDataToVerts edges = vertsFromEdgeSeq edgeWithDataToVerts edges
    let inline multiEdgesToVerts multiEdges = vertsFromEdgeSeq multiEdgeToVerts multiEdges
    let inline multiEdgesWithDataToVerts multiEdges = vertsFromEdgeSeq multiEdgeWithDataToVerts multiEdges