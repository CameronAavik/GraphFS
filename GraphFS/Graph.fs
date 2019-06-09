namespace GraphFs

open System.Collections.Generic

module DictHelpers =
    let inline private _GetOrDefault f key ``default`` (d : Dictionary<'a, 'b>) =
        match d.TryGetValue key with
        | true, value -> value |> f
        | false, _ -> ``default``

    let inline GetOrDefault k1 ``default`` d = _GetOrDefault id k1 ``default`` d
    let inline GetOrDefault2 (k1, k2) ``default`` d = _GetOrDefault (GetOrDefault k2 ``default``) k1 ``default`` d
    let inline GetOrDefault3 (k1, k2, k3) ``default`` d = _GetOrDefault (GetOrDefault2 (k2, k3) ``default``) k1 ``default`` d

    let inline GetOrThrow k1 (d : Dictionary<'a, 'b>) = d.[k1]
    let inline GetOrThrow2 (k1, k2) (d : Dictionary<'a, Dictionary<'b, 'c>>) = d.[k1].[k2]
    let inline GetOrThrow3 (k1, k2, k3) (d : Dictionary<'a, Dictionary<'b, Dictionary<'c, 'd>>>) = d.[k1].[k2].[k3]

    let inline private _ContainsKey f key (d : Dictionary<'a, 'b>) =
        match d.TryGetValue key with
        | true, value -> value |> f
        | false, _ -> false

    let inline ContainsKey k1 (d : Dictionary<'a, 'b>) = d.ContainsKey k1
    let inline ContainsKey2 (k1, k2) d = _ContainsKey (ContainsKey k2) k1 d
    let inline ContainsKey3 (k1, k2, k3) d = _ContainsKey (ContainsKey2 (k2, k3)) k1 d
        
    let inline private GetOrSetDefault k ``default`` (d : Dictionary<'a, 'b>) =
        match d.TryGetValue k with
        | true, value -> value
        | false, _ ->
            d.[k] <- ``default``
            ``default``

    let inline private _Add f k v d =
        let d2 = GetOrSetDefault k (new Dictionary<'K, 'V>()) d
        f v d2
    
    let inline Add k v (d : Dictionary<'a, 'b>) = d.[k] <- v
    let inline Add2 (k1, k2) v d = _Add (Add k2) k1 v d 
    let inline Add3 (k1, k2, k3) v d = _Add (Add2 (k2, k3)) k1 v d

    let inline private _ApplyOrEmptySeq f key (d : Dictionary<'a, 'b>) =
        match d.TryGetValue key with
        | true, value -> value |> f
        | false, _ -> Seq.empty

    let inline Keys (d : Dictionary<'a, 'b>) : 'a seq = upcast d.Keys
    let inline Keys2 k d = _ApplyOrEmptySeq Keys k d
    let inline Keys3 (k1, k2) d = _ApplyOrEmptySeq (Keys2 k2) k1 d

    let inline AsTupleSeq (d : Dictionary<'a, 'b>) = d |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
    let inline AsTupleSeq2 k d = _ApplyOrEmptySeq AsTupleSeq k d
    let inline AsTupleSeq3 (k1, k2) d = _ApplyOrEmptySeq (AsTupleSeq2 k2) k1 d

    let inline FromTupleSeq edges =
        let d = new Dictionary<'K, 'V>()
        for (u, data) in edges do
            Add u data d
        d

    let inline FromTupleSeq2 edges =
        let d = new Dictionary<'K, 'V>()
        for (u, v, data) in edges do
            Add2 (u, v) data d
        d

    let inline FromTupleSeq3 edges =
        let d = new Dictionary<'K, 'V>()
        for (u, v, i, data) in edges do
            Add3 (u, v, i) data d
        d

module MapHelpers =
    let inline private _GetOrDefault f key ``default`` (d : Map<'a, 'b>) =
        match d.TryGetValue key with
        | true, value -> value |> f
        | false, _ -> ``default``

    let inline GetOrDefault k1 ``default`` d = _GetOrDefault id k1 ``default`` d
    let inline GetOrDefault2 (k1, k2) ``default`` d = _GetOrDefault (GetOrDefault k2 ``default``) k1 ``default`` d
    let inline GetOrDefault3 (k1, k2, k3) ``default`` d = _GetOrDefault (GetOrDefault2 (k2, k3) ``default``) k1 ``default`` d

    let inline GetOrThrow k1 (d : Map<'a, 'b>) = d.[k1]
    let inline GetOrThrow2 (k1, k2) (d : Map<'a, Map<'b, 'c>>) = d.[k1].[k2]
    let inline GetOrThrow3 (k1, k2, k3) (d : Map<'a, Map<'b, Map<'c, 'd>>>) = d.[k1].[k2].[k3]

    let inline private _ContainsKey f key (d : Map<'a, 'b>) =
        match d.TryGetValue key with
        | true, value -> value |> f
        | false, _ -> false

    let inline ContainsKey k1 (d : Map<'a, 'b>) = d.ContainsKey k1
    let inline ContainsKey2 (k1, k2) d = _ContainsKey (ContainsKey k2) k1 d
    let inline ContainsKey3 (k1, k2, k3) d = _ContainsKey (ContainsKey2 (k2, k3)) k1 d

    let inline private _Add f k v d =
        let d2 = GetOrDefault k Map.empty d
        Map.add k (f v d2) d
    
    let inline Add k v d = Map.add k v d
    let inline Add2 (k1, k2) v d = _Add (Add k2) k1 v d 
    let inline Add3 (k1, k2, k3) v d = _Add (Add2 (k2, k3)) k1 v d

    let inline private _Remove f k d =
        let d2 = GetOrDefault k Map.empty d
        Map.add k (f d2) d

    let inline Remove k d = Map.remove k d
    let inline Remove2 (k1, k2) d = _Remove (Remove k2) k1 d 
    let inline Remove3 (k1, k2, k3) d = _Remove (Remove2 (k2, k3)) k1 d

    let inline RemoveFromFirst2Keys k d = d |> Remove k |> Map.map (fun _ d -> Remove k d)

    let inline private _ApplyOrEmptySeq f key (d : Map<'a, 'b>) =
        match d.TryGetValue key with
        | true, value -> value |> f
        | false, _ -> Seq.empty

    let inline Keys (d : Map<'a, 'b>) : 'a seq = Map.toSeq d |> Seq.map fst
    let inline Keys2 k d = _ApplyOrEmptySeq Keys k d
    let inline Keys3 (k1, k2) d = _ApplyOrEmptySeq (Keys2 k2) k1 d

    let inline AsTupleSeq (d : Map<'a, 'b>) = d |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
    let inline AsTupleSeq2 k d = _ApplyOrEmptySeq AsTupleSeq k d
    let inline AsTupleSeq3 (k1, k2) d = _ApplyOrEmptySeq (AsTupleSeq2 k2) k1 d

    let inline FromTupleSeq edges = Map.ofSeq edges

    let inline FromTupleSeq2 edges =
        edges
        |> Seq.groupBy (fun (u, _, _) -> u)
        |> Seq.map (fun (u, e) -> (u, Seq.map (fun (_, v, i) -> (v, i)) e |> FromTupleSeq))
        |> Map.ofSeq

    let inline FromTupleSeq3 edges =
        edges
        |> Seq.groupBy (fun (u, _, _ , _) -> u)
        |> Seq.map (fun (u, e) -> (u, Seq.map (fun (_, v, i, d) -> (v, i, d)) e |> FromTupleSeq2))
        |> Map.ofSeq

    let inline Flatten d = Map.toSeq d
    let inline Flatten2 d = Map.toSeq d |> Seq.collect (fun (u, d2) -> Flatten d2 |> Seq.map (fun (v, i) -> (u, v, i)))
    let inline Flatten3 d = Map.toSeq d |> Seq.collect (fun (u, d2) -> Flatten2 d2 |> Seq.map (fun (v, i, e) -> (u, v, i, e)))

module Edge =
    let inline edgeToVerts (u, v) = [|u; v|]
    let inline edgeWithDataToVerts ((u, v), _) = [|u; v|]
    let inline multiEdgeToVerts (u, v, _) = [|u; v|]
    let inline multiEdgeWithDataToVerts ((u, v, _), _) = [|u; v|]

    let inline vertsFromEdgeSeq (toVerts : 'a -> 'b array) edges=
        let vertSet = new HashSet<'b>()
        for edge in edges do
            let vertArr = toVerts edge
            vertSet.Add vertArr.[0] |> ignore
            vertSet.Add vertArr.[1] |> ignore
        vertSet

    let inline edgesToVerts edges =  vertsFromEdgeSeq edgeToVerts edges
    let inline edgesWithDataToVerts edges = vertsFromEdgeSeq edgeWithDataToVerts edges
    let inline multiEdgesToVerts multiEdges = vertsFromEdgeSeq multiEdgeToVerts multiEdges
    let inline multiEdgesWithDataToVerts multiEdges = vertsFromEdgeSeq multiEdgeWithDataToVerts multiEdges

module VertexSet =
    type IVertexSet<'V> =
        abstract member HasVert : 'V -> bool

    type IFiniteVertexSet<'V> =
        inherit IVertexSet<'V>
        abstract member Verts : 'V seq

    type FrozenVertexSet<'V>(verts : HashSet<'V>) =
        interface IFiniteVertexSet<'V> with
            member __.HasVert v = verts.Contains v
            member __.Verts = upcast verts

    module FrozenVertexSet =
        let ofSeq (verts : 'V seq) = new FrozenVertexSet<'V>(new HashSet<'V>(verts))
        let empty<'V> = new FrozenVertexSet<'V>(new HashSet<'V>())

    type VertexSet<'V when 'V : comparison>(verts : Set<'V>) =
        member __.AddVert vert = new VertexSet<'V>(verts.Add vert)
        member __.AddVerts toAdd = new VertexSet<'V>(verts + (set toAdd))
        member __.RemoveVert vert =  new VertexSet<'V>(verts.Remove vert)
        member __.RemoveVerts toRemove = new VertexSet<'V>(verts - (set toRemove))
        member __.Freeze () = FrozenVertexSet.ofSeq verts
        interface IFiniteVertexSet<'V> with
            member __.HasVert v = verts.Contains v
            member __.Verts = upcast verts

    module VertexSet =
        let ofSeq (verts : 'V seq) =  new VertexSet<'V>(set verts)
        let empty<'V when 'V : comparison> =  new VertexSet<'V>(Set.empty)

        let inline hasVert vertex (vertexSet : IVertexSet<'V>) = vertexSet.HasVert vertex
        let inline verts (vertexSet : IFiniteVertexSet<'V>) = vertexSet.Verts
        let inline addVert vert vertexSet = (^VS : (member AddVert : 'V -> ^VS) (vertexSet, vert))
        let inline addVerts verts vertexSet = (^VS : (member AddVerts : 'V seq -> ^VS) (vertexSet, verts))
        let inline removeVert vert vertexSet = (^VS : (member RemoveVert : 'V -> ^VS) (vertexSet, vert))
        let inline removeVerts verts vertexSet = (^VS : (member RemoveVerts : 'V seq -> ^VS) (vertexSet, verts))
        let inline freeze vertexSet = (^VS : (member Freeze : unit -> ^FVS) vertexSet)

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
        let ofUndirectedSeq edges =
            edges
            |> Seq.filter (fun (a, b, _) -> a <> b)
            |> Seq.map (fun (a, b, d) -> (b, a, d))
            |> Seq.append edges
            |> ofSeq
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
        let ofUndirectedSeq edges =
            edges
            |> Seq.filter (fun (a, b) -> a <> b)
            |> Seq.map (fun (a, b) -> (b, a))
            |> Seq.append edges
            |> ofSeq
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
        let inline addEdges edges edgeSet = (^ES : (member AddEdge : ('V * 'V) seq -> ^ES) (edgeSet, edges))
        let inline addEdgeWithData edge data edgeSet = (^ES : (member AddEdgeWithData : (('V * 'V) * 'E) -> ^ES) (edgeSet, (edge, data)))
        let inline addEdgesWithData edges edgeSet = (^ES : (member AddEdgesWithData : (('V * 'V) * 'E) seq -> ^ES) (edgeSet, edges))
        let inline removeEdge edge edgeSet = (^ES : (member RemoveEdge : ('V * 'V) -> ^ES) (edgeSet, edge))
        let inline removeEdges edge edgeSet = (^ES : (member RemoveEdges : ('V * 'V) -> ^ES) (edgeSet, edge))
        let inline freeze edgeSet = (^ES : (member Freeze : unit -> ^FES) edgeSet)

module MultiEdgeSet =
    open EdgeSet

    type IMultiEdgeSet<'T> =
        inherit IEdgeSet<'T>
        abstract member Edges : 'T -> ('T * int) seq
        abstract member HasMultiEdge : ('T * 'T * int) -> bool
        abstract member ParallelEdgeCount : ('T * 'T) -> int

    type IMultiEdgeWithDataSet<'T, 'E> =
        inherit IMultiEdgeSet<'T>
        inherit IEdgeWithDataSet<'T, IReadOnlyDictionary<int, 'E>>
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

module Graph =
    open VertexSet
    open EdgeSet
    open MultiEdgeSet

    type Graph<'V, 'VS, 'ES when 'VS :> VertexSet.IVertexSet<'V> and 'ES :> EdgeSet.IEdgeSet<'V>> = Graph of 'VS * 'ES with
        member this.V = match this with Graph (V, E) -> V
        member this.E = match this with Graph (V, E) -> E
        static member applyV f (g : Graph<'V, 'VS, 'ES>) = f g.V
        static member applyE f (g : Graph<'V, 'VS, 'ES>) = f g.E
        static member mapV f (g : Graph<'V, 'VS, 'ES>) = Graph (f g.V, g.E)
        static member mapE f (g : Graph<'V, 'VS, 'ES>) = Graph (g.V, f g.E)
        static member mapVE f1 f2 (g : Graph<'V, 'VS, 'ES>) = Graph (f1 g.V, f2 g.E)

    module Graph =
        let inline hasVert vert G = Graph.applyV (VertexSet.hasVert vert) G
        let inline verts G = Graph.applyV VertexSet.verts G
        let inline neighbours vert G  = Graph.applyE (EdgeSet.neighbours vert) G
        let inline hasEdge edge G = Graph.applyE (EdgeSet.hasEdge edge) G
        let inline neighboursWithData vert G = Graph.applyE (EdgeSet.neighboursWithData vert) G
        let inline getEdgeData edge G = Graph.applyE (EdgeSet.getEdgeData edge) G
        let inline addVert vert G = Graph.mapV (VertexSet.addVert vert) G
        let inline addVerts verts G = Graph.mapV (VertexSet.addVerts verts) G
        let inline removeVert vert G = Graph.mapVE (VertexSet.removeVert vert) (EdgeSet.removeVert vert) G
        let inline removeVerts verts G = Graph.mapVE (VertexSet.removeVerts verts) (EdgeSet.removeVerts verts) G
        let inline addEdge edge G = Graph.mapVE (VertexSet.addVerts (Edge.edgeToVerts edge)) (EdgeSet.addEdge edge) G
        let inline addEdges edges G = Graph.mapVE (VertexSet.addVerts (Edge.edgesToVerts edges)) (EdgeSet.addEdges edges) G
        let inline addEdgeWithData edge data G = Graph.mapVE (VertexSet.addVerts (Edge.edgeToVerts edge)) (EdgeSet.addEdgeWithData edge data) G
        let inline addEdgesWithData edges G = Graph.mapVE (VertexSet.addVerts (Edge.edgesWithDataToVerts edges)) (EdgeSet.addEdgesWithData edges) G
        let inline removeEdge edge G = Graph.mapE (EdgeSet.removeEdge edge) G
        let inline removeEdges edges G = Graph.mapE (EdgeSet.removeEdges edges) G
        let inline freeze G = Graph.mapVE VertexSet.freeze EdgeSet.freeze G
        
        let empty<'V when 'V : comparison> = Graph (VertexSet.empty<'V>, EdgeSet.empty<'V>)
        let emptyWithEdgeData<'V, 'E when 'V : comparison> = Graph (VertexSet.empty<'V>, EdgeWithDataSet.empty<'V, 'E>)

    module MultiGraph =
        let inline edges edge G = Graph.applyE (MultiEdgeSet.edges edge) G
        let inline hasMultiEdge edge G = Graph.applyE (MultiEdgeSet.hasMultiEdge edge) G
        let inline parallelEdgeCount edge G = Graph.applyE (MultiEdgeSet.parallelEdgeCount edge) G
        let inline multiEdgesWithData vert G = Graph.applyE (MultiEdgeSet.multiEdgesWithData vert) G
        let inline getMultiEdgeData multiEdge G = Graph.applyE (MultiEdgeSet.getMultiEdgeData multiEdge) G
        let inline addMultiEdge multiEdge G = Graph.mapVE (VertexSet.addVerts (Edge.multiEdgeToVerts multiEdge)) (MultiEdgeSet.addMultiEdge multiEdge) G
        let inline addMultiEdges multiEdges G = Graph.mapVE (VertexSet.addVerts (Edge.multiEdgesToVerts multiEdges)) (MultiEdgeSet.addMultiEdges multiEdges) G
        let inline addMultiEdgeWithData multiEdge data G = Graph.mapVE (VertexSet.addVerts (Edge.multiEdgeToVerts multiEdge)) (MultiEdgeSet.addMultiEdgeWithData multiEdge data) G
        let inline addMultiEdgesWithData multiEdges G = Graph.mapVE (VertexSet.addVerts (Edge.multiEdgesWithDataToVerts multiEdges)) (MultiEdgeSet.addMultiEdgesWithData multiEdges) G
        let inline removeMultiEdge multiEdge G = Graph.mapE (MultiEdgeSet.removeMultiEdge multiEdge) G
        let inline removeMultiEdges multiEdges G = Graph.mapE (MultiEdgeSet.removeMultiEdges multiEdges) G
        
        let empty<'V when 'V : comparison> = Graph (VertexSet.empty<'V>, MultiEdgeSet.empty<'V>)
        let emptyWithEdgeData<'V, 'E when 'V : comparison> = Graph (VertexSet.empty<'V>, MultiEdgeWithDataSet.empty<'V, 'E>)

module Example = 
    open Graph

    let testGraph = 
        Graph.emptyWithEdgeData<int, int>
        |> Graph.addVert 2
        |> Graph.addVerts [|2; 3|]
        |> Graph.addEdgeWithData (1, 2) 3
