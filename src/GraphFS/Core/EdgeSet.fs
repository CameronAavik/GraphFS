namespace GraphFS.Core

open System.Collections.Generic
open GraphFS.Helpers

module EdgeSet =
    type IEdgeSet<'V> =
        abstract member Neighbours : 'V -> 'V seq
        abstract member HasEdge : ('V * 'V) -> bool

    type IEdgeWithDataSet<'V, 'E> =
        inherit IEdgeSet<'V>
        abstract member NeighboursWithData : 'V -> ('V * 'E) seq
        abstract member GetEdgeData : ('V * 'V) -> 'E

    type FrozenEdgeWithDataSet<'V, 'E>(edges : Dictionary<'V, Dictionary<'V, 'E>>) =
        interface IEdgeWithDataSet<'V, 'E> with
            member __.Neighbours n = DictHelpers.Keys2 n edges
            member __.HasEdge neighbour = DictHelpers.ContainsKey2 neighbour edges
            member __.NeighboursWithData n = DictHelpers.AsTupleSeq2 n edges
            member __.GetEdgeData neighbour = DictHelpers.GetOrThrow2 neighbour edges

    module FrozenEdgeWithDataSet =
        let ofSeq edges = edges |> DictHelpers.FromTupleSeq2 |> FrozenEdgeWithDataSet<'V, 'E>
        let empty<'V, 'E when 'V : equality> = new FrozenEdgeWithDataSet<'V, 'E>(new Dictionary<'V, Dictionary<'V, 'E>>())

    type FrozenEdgeSet<'V>(edges : Dictionary<'V, Dictionary<'V, unit>>) =
        inherit FrozenEdgeWithDataSet<'V, unit>(edges)
        interface IEdgeSet<'V>

    module FrozenEdgeSet =
        let ofSeq edges =
            edges
            |> Seq.map (fun (u, v) -> (u, v, ()))
            |> DictHelpers.FromTupleSeq2
            |> FrozenEdgeSet<'V>
        let empty<'V when 'V : equality> = new FrozenEdgeSet<'V>(new Dictionary<'V, Dictionary<'V, unit>>())

    type EdgeWithDataSet<'V, 'E when 'V : comparison>(edges : Map<'V, Map<'V, 'E>>) =
        member __.RemoveVert vert = MapHelpers.RemoveFromFirst2Keys vert edges |> EdgeWithDataSet<'V, 'E>
        member es.RemoveVerts verts = (Seq.fold (fun (es : EdgeWithDataSet<'V, 'E>) n -> es.RemoveVert n) es verts)
        member __.AddEdgeWithData edgeWithData =
            let edge, data = edgeWithData
            MapHelpers.Add2 edge data edges |> EdgeWithDataSet<'V, 'E>
        member es.AddEdgesWithData edges = (Seq.fold (fun (es : EdgeWithDataSet<'V, 'E>) (edge, data) -> es.AddEdgeWithData (edge, data)) es edges)
        member __.RemoveEdge edge = MapHelpers.Remove2 edge edges |> EdgeWithDataSet<'V, 'E>
        member es.RemoveEdges neighbours = (Seq.fold (fun (es : EdgeWithDataSet<'V, 'E>) n -> es.RemoveEdge n) es neighbours)
        member __.Freeze () = (MapHelpers.Flatten2 edges |> FrozenEdgeWithDataSet.ofSeq)
        interface IEdgeWithDataSet<'V, 'E> with
            member __.Neighbours n = MapHelpers.Keys2 n edges
            member __.HasEdge neighbour = MapHelpers.ContainsKey2 neighbour edges
            member __.NeighboursWithData n = MapHelpers.AsTupleSeq2 n edges
            member __.GetEdgeData neighbour = MapHelpers.GetOrThrow2 neighbour edges

    module EdgeWithDataSet =
        let ofSeq edges = edges |> MapHelpers.FromTupleSeq2 |> EdgeWithDataSet<'V, 'E>
        let empty<'V, 'E when 'V : comparison> = new EdgeWithDataSet<'V, 'E>(Map.empty)

    type EdgeSet<'V when 'V : comparison>(edges : Map<'V, Map<'V, unit>>) =
        member __.RemoveVert vert = MapHelpers.RemoveFromFirst2Keys vert edges |> EdgeSet<'V>
        member es.RemoveVerts verts = (Seq.fold (fun (es : EdgeSet<'V>) n -> es.RemoveVert n) es verts)
        member __.AddEdge edge = MapHelpers.Add2 edge () edges |> EdgeSet<'V>
        member es.AddEdges edge = Seq.fold (fun (es : EdgeSet<'V>) n -> es.AddEdge n) es edge
        member __.RemoveEdge neighbour = MapHelpers.Remove2 neighbour edges |> EdgeSet<'V>
        member es.RemoveEdges neighbours = Seq.fold (fun (es : EdgeSet<'V>) n -> es.RemoveEdge n) es neighbours
        member __.Freeze () = MapHelpers.Flatten2 edges |> Seq.map (fun (u, v, _) -> (u, v)) |> FrozenEdgeSet.ofSeq
        interface IEdgeSet<'V> with
            member __.Neighbours n = MapHelpers.Keys2 n edges
            member __.HasEdge neighbour = MapHelpers.ContainsKey2 neighbour edges

    module EdgeSet =
        let ofSeq edges =
            edges
            |> Seq.map (fun (u, v) -> (u, v, ()))
            |> MapHelpers.FromTupleSeq2
            |> EdgeSet<'V>
        let empty<'V when 'V : comparison> = new EdgeSet<'V>(Map.empty)

        let inline neighbours vertex (edgeSet : IEdgeSet<'V>) = edgeSet.Neighbours vertex
        let inline hasEdge edge (edgeSet : IEdgeSet<'V>) = edgeSet.HasEdge edge
        let inline neighboursWithData vertex (edgeSet : IEdgeWithDataSet<'V, 'E>) = edgeSet.NeighboursWithData vertex
        let inline getEdgeData edge (edgeSet : IEdgeWithDataSet<'V, 'E>) = edgeSet.GetEdgeData edge
        let inline removeVert vert edgeSet = (^ES : (member RemoveVert : 'V -> ^ES) (edgeSet, vert))
        let inline removeVerts verts edgeSet = (^ES : (member RemoveVerts : 'V seq -> ^ES) (edgeSet, verts))
        let inline addEdge edge edgeSet = (^ES : (member AddEdge : ('V * 'V) -> ^ES) (edgeSet, edge))
        let inline addEdges edges edgeSet = (^ES : (member AddEdges : ('V * 'V) seq -> ^ES) (edgeSet, edges))
        let inline addEdgeWithData edge data edgeSet = (^ES : (member AddEdgeWithData : (('V * 'V) * 'E) -> ^ES) (edgeSet, (edge, data)))
        let inline addEdgesWithData edges edgeSet = (^ES : (member AddEdgesWithData : (('V * 'V) * 'E) seq -> ^ES) (edgeSet, edges))
        let inline removeEdge edge edgeSet = (^ES : (member RemoveEdge : ('V * 'V) -> ^ES) (edgeSet, edge))
        let inline removeEdges edge edgeSet = (^ES : (member RemoveEdges : ('V * 'V) -> ^ES) (edgeSet, edge))
        let inline freeze edgeSet = (^ES : (member Freeze : unit -> ^FES) edgeSet)