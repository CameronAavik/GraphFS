namespace GraphFS.Core

open System.Collections.Generic
open GraphFS.Helpers

module MultiEdgeSet =
    type IMultiEdgeSet<'T> =
        inherit EdgeSet.IEdgeSet<'T>
        abstract member Edges : 'T -> ('T * int) seq
        abstract member HasMultiEdge : ('T * 'T * int) -> bool
        abstract member ParallelEdgeCount : ('T * 'T) -> int

    type IMultiEdgeWithDataSet<'T, 'E> =
        inherit IMultiEdgeSet<'T>
        inherit EdgeSet.IEdgeWithDataSet<'T, IReadOnlyDictionary<int, 'E>>
        abstract member MultiEdgesWithData : 'T -> ('T * int * 'E) seq
        abstract member GetMultiEdgeData : ('T * 'T * int) -> 'E

    type FrozenMultiEdgeWithDataSet<'V, 'E>(edges : Dictionary<'V, Dictionary<'V, Dictionary<int, 'E>>>) =
        interface IMultiEdgeWithDataSet<'V, 'E> with
            member __.Neighbours n = DictHelpers.Keys2 n edges
            member __.HasEdge neighbour = DictHelpers.ContainsKey2 neighbour edges
            member __.Edges n = DictHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> Seq.map (fun i -> (k, i)) d.Keys)
            member __.HasMultiEdge e = DictHelpers.ContainsKey3 e edges
            member __.ParallelEdgeCount n =
                match DictHelpers.GetOrDefault2 n null edges with
                | null -> 0
                | d -> d.Count
            member __.NeighboursWithData n = DictHelpers.AsTupleSeq2 n edges |> Seq.map (fun (k, v) -> (k, upcast v))
            member __.GetEdgeData n = upcast (DictHelpers.GetOrThrow2 n edges)
            member __.MultiEdgesWithData n = DictHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> d |> Seq.map (fun kvp -> (k, kvp.Key, kvp.Value)))
            member __.GetMultiEdgeData e = DictHelpers.GetOrThrow3 e edges

    module FrozenMultiEdgeWithDataSet =
        let private edgesToMultiEdges edges =
            edges
            |> Seq.groupBy (fun (u, v, _) -> (u, v))
            |> Seq.collect (fun ((u, v), s) -> Seq.mapi (fun i (_, _, d) -> (u, v, i, d)) s)
        let ofSeq (multiEdges : ('V * 'V * int * 'E) seq) = multiEdges |> DictHelpers.FromTupleSeq3 |> FrozenMultiEdgeWithDataSet<'V, 'E>
        let ofEdgeSeq (edges : ('V * 'V * 'E) seq) = edges |> edgesToMultiEdges |> ofSeq
        let ofUndirectedSeq multiEdges =
            multiEdges
            |> Seq.filter (fun (a, b, _, _) -> a <> b)
            |> Seq.map (fun (a, b, i, d) -> (b, a, i, d))
            |> Seq.append multiEdges
            |> ofSeq
        let ofUndirectedEdgeSeq edges = edges |> edgesToMultiEdges |> ofUndirectedSeq
        let empty<'V, 'E when 'V : equality> = new FrozenMultiEdgeWithDataSet<'V, 'E>(new Dictionary<_,_>())
        
    type FrozenMultiEdgeSet<'V>(edges : Dictionary<'V, Dictionary<'V, Dictionary<int, unit>>>) =
        inherit FrozenMultiEdgeWithDataSet<'V, unit>(edges)
        interface IMultiEdgeSet<'V>

    module FrozenMultiEdgeSet =
        let private edgesToMultiEdges edges =
            edges
            |> Seq.groupBy id
            |> Seq.collect (fun ((u, v), s) -> Seq.mapi (fun i _ -> (u, v, i)) s)
        let ofSeq (multiEdges : ('V * 'V * int) seq) =
            multiEdges
            |> Seq.map (fun (u, v, i) -> (u, v, i, ()))
            |> DictHelpers.FromTupleSeq3
            |> FrozenMultiEdgeSet<'V>
        let ofEdgeSeq (edges : ('V * 'V) seq) = edges |> edgesToMultiEdges |> ofSeq
        let ofUndirectedSeq multiEdges =
            multiEdges
            |> Seq.filter (fun (a, b, _) -> a <> b)
            |> Seq.map (fun (a, b, i) -> (b, a, i))
            |> Seq.append multiEdges
            |> ofSeq
        let ofUndirectedEdgeSeq edges = edges |> edgesToMultiEdges |> ofUndirectedSeq
        let empty<'V when 'V : equality> = new FrozenMultiEdgeSet<'V>(new Dictionary<_,_>())

    type MultiEdgeWithDataSet<'V, 'E when 'V : comparison>(edges : Map<'V, Map<'V, Map<int, 'E>>>) =
        member __.RemoveVert vert = MapHelpers.RemoveFromFirst2Keys vert edges |> MultiEdgeWithDataSet<'V, 'E>
        member es.RemoveVerts verts = Seq.fold (fun (es : MultiEdgeWithDataSet<'V, 'E>) n -> es.RemoveVert n) es verts
        member __.AddEdgeWithData edgeWithData =
            let ((u, v), d) = edgeWithData
            let indexes = MapHelpers.Keys3 (u, v) edges
            let nextIndex = if Seq.isEmpty indexes then 0 else (Seq.max indexes) + 1
            MapHelpers.Add3 (u, v, nextIndex) d edges |> MultiEdgeWithDataSet<'V, 'E>
        member es.AddEdgesWithData edges = Seq.fold (fun (es : MultiEdgeWithDataSet<'V, 'E>) ((u, v), d) -> es.AddEdgeWithData ((u, v), d)) es edges
        member __.RemoveEdge edge = MapHelpers.Remove2 edge edges |> MultiEdgeWithDataSet<'V, 'E>
        member es.RemoveEdges edges = Seq.fold (fun (es : MultiEdgeWithDataSet<'V, 'E>) n -> es.RemoveEdge n) es edges
        member __.AddMultiEdgeWithData multiEdgeWithData =
            let multiEdge, data = multiEdgeWithData
            MapHelpers.Add3 multiEdge data edges |> MultiEdgeWithDataSet<'V, 'E>
        member es.AddMultiEdgesWithData multiEdges = Seq.fold (fun (es : MultiEdgeWithDataSet<'V, 'E>) (multiEdge, data) -> es.AddMultiEdgeWithData (multiEdge, data)) es multiEdges
        member __.RemoveMultiEdge multiEdge = MapHelpers.Remove3 multiEdge edges |> MultiEdgeWithDataSet<'V, 'E>
        member es.RemoveMultiEdges multiEdges = Seq.fold (fun (es : MultiEdgeWithDataSet<'V, 'E>) n -> es.RemoveMultiEdge n) es multiEdges
        member __.Freeze () = MapHelpers.Flatten3 edges |> FrozenMultiEdgeWithDataSet.ofSeq
        interface IMultiEdgeWithDataSet<'V, 'E> with
            member __.Neighbours n = MapHelpers.Keys2 n edges
            member __.HasEdge neighbour = MapHelpers.ContainsKey2 neighbour edges
            member __.Edges n = MapHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> d |> Map.toSeq |> Seq.map (fun (i, _) -> (k, i)))
            member __.HasMultiEdge e = MapHelpers.ContainsKey3 e edges
            member __.ParallelEdgeCount n = (MapHelpers.GetOrDefault2 n Map.empty edges).Count
            member __.NeighboursWithData n = MapHelpers.AsTupleSeq2 n edges |> Seq.map (fun (k, v) -> (k, upcast v))
            member __.GetEdgeData n = upcast (MapHelpers.GetOrThrow2 n edges)
            member __.MultiEdgesWithData n = MapHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> d |> Seq.map (fun kvp -> (k, kvp.Key, kvp.Value)))
            member __.GetMultiEdgeData e = MapHelpers.GetOrThrow3 e edges

    module MultiEdgeWithDataSet =
        let private edgesToMultiEdges edges =
            edges
            |> Seq.groupBy (fun (u, v, _) -> (u, v))
            |> Seq.collect (fun ((u, v), s) -> Seq.mapi (fun i (_, _, d) -> (u, v, i, d)) s)
        let ofSeq multiEdges = multiEdges |> MapHelpers.FromTupleSeq3 |> MultiEdgeWithDataSet<'V, 'E>
        let ofEdgeSeq edges = edges |> edgesToMultiEdges |> ofSeq
        let ofUndirectedSeq multiEdges =
            multiEdges
            |> Seq.filter (fun (a, b, _, _) -> a <> b)
            |> Seq.map (fun (a, b, i, d) -> (b, a, i, d))
            |> Seq.append multiEdges
            |> ofSeq
        let ofUndirectedEdgeSeq edges = edges |> edgesToMultiEdges |> ofUndirectedSeq
        let empty<'V, 'E when 'V : comparison> = new MultiEdgeWithDataSet<'V, 'E>(Map.empty)
        
    type MultiEdgeSet<'V when 'V : comparison>(edges : Map<'V, Map<'V, Map<int, unit>>>) =
        member __.RemoveVert vert = MapHelpers.RemoveFromFirst2Keys vert edges |> MultiEdgeSet<'V>
        member es.RemoveVerts verts = (Seq.fold (fun (es : MultiEdgeSet<'V>) n -> es.RemoveVert n) es verts)
        member __.AddEdge edge = 
            let u, v = edge
            let indexes = MapHelpers.Keys3 (u, v) edges
            let nextIndex = if Seq.isEmpty indexes then 0 else (Seq.max indexes) + 1
            MapHelpers.Add3 (u, v, nextIndex) () edges |> MultiEdgeSet<'V>
        member es.AddEdges edges = (Seq.fold (fun (es : MultiEdgeSet<'V>) n -> es.AddEdge n) es edges)
        member __.RemoveEdge edge = MapHelpers.Remove2 edge edges |> MultiEdgeSet<'V>
        member es.RemoveEdges edges = (Seq.fold (fun (es : MultiEdgeSet<'V>) n -> es.RemoveEdges n) es edges)
        member __.AddMultiEdge multiEdge = MapHelpers.Add3 multiEdge () edges |> MultiEdgeSet<'V>
        member es.AddMultiEdges edges = (Seq.fold (fun (es : MultiEdgeSet<'V>) n -> es.AddMultiEdge n) es edges)
        member __.RemoveMultiEdge edge = MapHelpers.Remove3 edge edges |> MultiEdgeSet<'V>
        member es.RemoveMultiEdges edges = (Seq.fold (fun (es : MultiEdgeSet<'V>) n -> es.RemoveEdge n) es edges)
        member __.Freeze () = (MapHelpers.Flatten3 edges |> Seq.map (fun (u, v, i, _) -> (u, v, i))  |> FrozenMultiEdgeSet.ofSeq)
        interface IMultiEdgeSet<'V> with
            member __.Neighbours n = MapHelpers.Keys2 n edges
            member __.HasEdge neighbour = MapHelpers.ContainsKey2 neighbour edges
            member __.Edges n = MapHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> d |> Map.toSeq |> Seq.map (fun (i, _) -> (k, i)))
            member __.HasMultiEdge e = MapHelpers.ContainsKey3 e edges
            member __.ParallelEdgeCount n = (MapHelpers.GetOrDefault2 n Map.empty edges).Count

    module MultiEdgeSet =
        let private edgesToMultiEdges edges =
            edges
            |> Seq.groupBy id
            |> Seq.collect (fun ((u, v), s) -> Seq.mapi (fun i _ -> (u, v, i)) s)
        let ofSeq (multiEdges : ('V * 'V * int) seq) = 
            multiEdges
            |> Seq.map (fun (u, v, i) -> (u, v, i, ()))
            |> MapHelpers.FromTupleSeq3
            |> MultiEdgeSet<'V>
        let ofEdgeSeq (edges : ('V * 'V) seq) = edges |> edgesToMultiEdges |> ofSeq
        let ofUndirectedSeq multiEdges =
            multiEdges
            |> Seq.filter (fun (a, b, _) -> a <> b)
            |> Seq.map (fun (a, b, i) -> (b, a, i))
            |> Seq.append multiEdges
            |> ofSeq
        let ofUndirectedEdgeSeq edges = edges |> edgesToMultiEdges |> ofUndirectedSeq
        let empty<'V when 'V : comparison> = new MultiEdgeSet<'V>(Map.empty)
        
        let inline edges vertex (edgeSet : IMultiEdgeSet<'V>) = edgeSet.Edges vertex
        let inline hasMultiEdge multiEdge (edgeSet : IMultiEdgeSet<'V>) = edgeSet.HasMultiEdge multiEdge
        let inline parallelEdgeCount edge (edgeSet : IMultiEdgeSet<'V>) = edgeSet.ParallelEdgeCount edge
        let inline multiEdgesWithData vertex (edgeSet : IMultiEdgeWithDataSet<'V, 'E>) = edgeSet.MultiEdgesWithData vertex
        let inline getMultiEdgeData multiEdge (edgeSet : IMultiEdgeWithDataSet<'V, 'E>) = edgeSet.GetMultiEdgeData multiEdge

        let inline addMultiEdge edge edgeSet = (^ES : (member AddMultiEdge : ('V * 'V * int) -> ^ES) (edgeSet, edge))
        let inline addMultiEdges edges edgeSet = (^ES : (member AddMultiEdge : ('V * 'V * int) seq -> ^ES) (edgeSet, edges))
        let inline addMultiEdgeWithData edge data edgeSet = (^ES : (member AddMultiEdgeWithData : (('V * 'V * int) * 'E) -> ^ES) (edgeSet, (edge, data)))
        let inline addMultiEdgesWithData edges edgeSet = (^ES : (member AddMultiEdgesWithData : (('V * 'V * int) * 'E) seq -> ^ES) (edgeSet, edges))
        let inline removeMultiEdge edge edgeSet = (^ES : (member RemoveMultiEdge : ('V * 'V * int) -> ^ES) (edgeSet, edge))
        let inline removeMultiEdges edge edgeSet = (^ES : (member RemoveMultiEdges : ('V * 'V * int) -> ^ES) (edgeSet, edge))
