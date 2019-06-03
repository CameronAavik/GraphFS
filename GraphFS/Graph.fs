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
        let d2 = GetOrSetDefault k (new Dictionary<_, _>()) d
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
        let d = new Dictionary<_, _>()
        for (u, data) in edges do
            Add u data d
        d

    let inline FromTupleSeq2 edges =
        let d = new Dictionary<_, _>()
        for (u, v, data) in edges do
            Add2 (u, v) data d
        d

    let inline FromTupleSeq3 edges =
        let d = new Dictionary<_, _>()
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

module NodeSet =
    type INodeSet<'T> =
        abstract member HasNode : 'T -> bool

    type IFiniteNodeSet<'T> =
        inherit INodeSet<'T>
        abstract member Nodes : 'T seq

    type IPersistentNodeSet<'T> =
        inherit IFiniteNodeSet<'T>
        abstract member AddNode : 'T -> IPersistentNodeSet<'T>
        abstract member AddNodes : 'T seq -> IPersistentNodeSet<'T>
        abstract member RemoveNode : 'T -> IPersistentNodeSet<'T>
        abstract member RemoveNodes : 'T seq -> IPersistentNodeSet<'T>
        abstract member AsFrozen : unit -> IFiniteNodeSet<'T>

    type FrozenNodeSet<'T>(nodes : HashSet<'T>) =
        interface IFiniteNodeSet<'T> with
            member __.HasNode n = nodes.Contains n
            member __.Nodes = upcast nodes
        static member create (nodes : 'T seq) = new FrozenNodeSet<_>(new HashSet<_>(nodes))
        static member empty = new FrozenNodeSet<_>(new HashSet<_>())

    type NodeSet<'T when 'T : comparison>(nodes : Set<'T>) =
        member __.AddNode n = new NodeSet<_>(nodes.Add n)
        member __.AddNodes toAdd = new NodeSet<_>(nodes + (set toAdd))
        member __.RemoveNode n =  new NodeSet<_>(nodes.Remove n)
        member __.RemoveNodes toRemove = new NodeSet<_>(nodes - (set toRemove))
        member __.AsFrozen () = FrozenNodeSet.create nodes
        interface IPersistentNodeSet<'T> with
            member __.HasNode n = nodes.Contains n
            member __.Nodes = upcast nodes
            member ns.AddNode n = upcast ns.AddNode n
            member ns.AddNodes toAdd = upcast ns.AddNodes toAdd
            member ns.RemoveNode n = upcast ns.RemoveNode n
            member ns.RemoveNodes toRemove = upcast ns.RemoveNodes toRemove
            member ns.AsFrozen () = upcast ns.AsFrozen ()
        static member ofSeq (nodes : 'T seq) =  new NodeSet<_>(set nodes)
        static member empty =  new NodeSet<_>(Set.empty)

module EdgeSet =
    type IEdgeSet<'T> =
        abstract member Neighbours : 'T -> 'T seq
        abstract member HasNeighbour : ('T * 'T) -> bool

    type IEdgeWithDataSet<'T, 'E> =
        inherit IEdgeSet<'T>
        abstract member NeighboursWithData : 'T -> ('T * 'E) seq
        abstract member GetNeighbourData : ('T * 'T) -> 'E

    type IPersistentEdgeSet<'T> =
        inherit IEdgeSet<'T>
        abstract member RemoveNode : 'T -> IPersistentEdgeSet<'T>
        abstract member RemoveNodes : 'T seq -> IPersistentEdgeSet<'T>
        abstract member AddNeighbour : ('T * 'T) -> IPersistentEdgeSet<'T>
        abstract member AddNeighbours : ('T * 'T) seq -> IPersistentEdgeSet<'T>
        abstract member RemoveNeighbour : ('T * 'T) -> IPersistentEdgeSet<'T>
        abstract member RemoveNeighbours : ('T * 'T) seq -> IPersistentEdgeSet<'T>
        abstract member AsFrozen : unit -> IEdgeSet<'T>

    type IPersistentEdgeWithDataSet<'T, 'E> =
        inherit IEdgeWithDataSet<'T, 'E>
        abstract member RemoveNode : 'T -> IPersistentEdgeWithDataSet<'T, 'E>
        abstract member RemoveNodes : 'T seq -> IPersistentEdgeWithDataSet<'T, 'E>
        abstract member AddNeighbour : ('T * 'T * 'E) -> IPersistentEdgeWithDataSet<'T, 'E>
        abstract member AddNeighbours : ('T * 'T * 'E) seq -> IPersistentEdgeWithDataSet<'T, 'E>
        abstract member RemoveNeighbour : ('T * 'T) -> IPersistentEdgeWithDataSet<'T, 'E>
        abstract member RemoveNeighbours : ('T * 'T) seq -> IPersistentEdgeWithDataSet<'T, 'E>
        abstract member AsFrozen : unit -> IEdgeWithDataSet<'T, 'E>

    type FrozenEdgeWithDataSet<'T, 'E>(edges : Dictionary<'T, Dictionary<'T, 'E>>) =
        interface IEdgeWithDataSet<'T, 'E> with
            member __.Neighbours n = DictHelpers.Keys2 n edges
            member __.HasNeighbour neighbour = DictHelpers.ContainsKey2 neighbour edges
            member __.NeighboursWithData n = DictHelpers.AsTupleSeq2 n edges
            member __.GetNeighbourData neighbour = DictHelpers.GetOrThrow2 neighbour edges
        static member ofUndirectedSeq edges =
            edges
            |> Seq.filter (fun (a, b, _) -> a <> b)
            |> Seq.map (fun (a, b, d) -> (b, a, d))
            |> Seq.append edges
            |> FrozenEdgeWithDataSet<'T, 'E>.ofSeq
        static member ofSeq edges = edges |> DictHelpers.FromTupleSeq2 |> FrozenEdgeWithDataSet<_, _>
        static member empty = new FrozenEdgeWithDataSet<_, _>(new Dictionary<_,_>())

    type FrozenEdgeSet<'T>(edges : Dictionary<'T, Dictionary<'T, unit>>) =
        inherit FrozenEdgeWithDataSet<'T, unit>(edges)
        interface IEdgeSet<'T>
        static member ofUndirectedSeq edges =
            edges
            |> Seq.filter (fun (a, b) -> a <> b)
            |> Seq.map (fun (a, b) -> (b, a))
            |> Seq.append edges
            |> FrozenEdgeSet<'T>.ofSeq
        static member ofSeq edges =
            edges
            |> Seq.map (fun (u, v) -> (u, v, ()))
            |> DictHelpers.FromTupleSeq2
            |> FrozenEdgeSet<_>
        static member empty = new FrozenEdgeSet<_>(new Dictionary<_,_>())

    type EdgeWithDataSet<'T, 'E when 'T : comparison>(edges : Map<'T, Map<'T, 'E>>) =
        member private __.RemoveNode node = MapHelpers.RemoveFromFirst2Keys node edges |> EdgeWithDataSet<_,_>
        member private __.AddNeighbour neighbour =
            let u, v, d = neighbour
            MapHelpers.Add2 (u, v) d edges |> EdgeWithDataSet<_,_>
        member private __.RemoveNeighbour neighbour = MapHelpers.Remove2 neighbour edges |> EdgeWithDataSet<_,_>
        interface IPersistentEdgeWithDataSet<'T, 'E> with
            member __.Neighbours n = MapHelpers.Keys2 n edges
            member __.HasNeighbour neighbour = MapHelpers.ContainsKey2 neighbour edges
            member es.RemoveNode node = upcast es.RemoveNode node
            member es.RemoveNodes nodes = upcast (Seq.fold (fun (es : EdgeWithDataSet<'T, 'E>) n -> es.RemoveNode n) es nodes)
            member es.AddNeighbour neighbour = upcast es.AddNeighbour neighbour
            member es.AddNeighbours neighbours = upcast (Seq.fold (fun (es : EdgeWithDataSet<'T, 'E>) n -> es.AddNeighbour n) es neighbours)
            member es.RemoveNeighbour neighbour = upcast es.RemoveNeighbour neighbour
            member es.RemoveNeighbours neighbours = upcast (Seq.fold (fun (es : EdgeWithDataSet<'T, 'E>) n -> es.RemoveNeighbour n) es neighbours)
            member __.AsFrozen () = upcast (MapHelpers.Flatten2 edges |> FrozenEdgeWithDataSet<_,_>.ofSeq)
            member __.NeighboursWithData n = MapHelpers.AsTupleSeq2 n edges
            member __.GetNeighbourData neighbour = MapHelpers.GetOrThrow2 neighbour edges
        static member ofSeq edges = edges |> MapHelpers.FromTupleSeq2 |> EdgeWithDataSet<_,_>
        static member empty = new EdgeWithDataSet<_, _>(Map.empty)

    type EdgeSet<'T when 'T : comparison>(edges : Map<'T, Map<'T, unit>>) =
        member private __.RemoveNode node = MapHelpers.RemoveFromFirst2Keys node edges |> EdgeSet<_>
        member private __.AddNeighbour neighbour = MapHelpers.Add2 neighbour () edges |> EdgeSet<_>
        member private __.RemoveNeighbour neighbour = MapHelpers.Remove2 neighbour edges |> EdgeSet<_>
        interface IPersistentEdgeSet<'T> with
            member __.Neighbours n = MapHelpers.Keys2 n edges
            member __.HasNeighbour neighbour = MapHelpers.ContainsKey2 neighbour edges
            member es.RemoveNode node = upcast es.RemoveNode node
            member es.RemoveNodes nodes = upcast (Seq.fold (fun (es : EdgeSet<_>) n -> es.RemoveNode n) es nodes)
            member es.AddNeighbour neighbour = upcast (es.AddNeighbour neighbour)
            member es.AddNeighbours neighbours = upcast (Seq.fold (fun (es : EdgeSet<_>) n -> es.AddNeighbour n) es neighbours)
            member es.RemoveNeighbour neighbour = upcast es.RemoveNeighbour neighbour
            member es.RemoveNeighbours neighbours = upcast (Seq.fold (fun (es : EdgeSet<_>) n -> es.RemoveNeighbour n) es neighbours)
            member __.AsFrozen () = upcast (MapHelpers.Flatten2 edges |> FrozenEdgeSet<_>.ofSeq)
        static member ofSeq edges =
            edges
            |> Seq.map (fun (u, v) -> (u, v, ()))
            |> MapHelpers.FromTupleSeq2
            |> EdgeSet<_>
        static member empty = new EdgeSet<_>(Map.empty)

module MultiEdgeSet =
    open EdgeSet

    type IMultiEdgeSet<'T> =
        inherit IEdgeSet<'T>
        abstract member Edges : 'T -> ('T * int) seq
        abstract member HasEdge : ('T * 'T * int) -> bool
        abstract member EdgeCount : ('T * 'T) -> int

    type IMultiEdgeWithDataSet<'T, 'E> =
        inherit IMultiEdgeSet<'T>
        inherit IEdgeWithDataSet<'T, IReadOnlyDictionary<int, 'E>>
        abstract member EdgesWithData : 'T -> ('T * int * 'E) seq
        abstract member GetEdgeData : ('T * 'T * int) -> 'E

    type IPersistentMultiEdgeSet<'T> =
        inherit IMultiEdgeSet<'T>
        abstract member RemoveNode : 'T -> IPersistentMultiEdgeSet<'T>
        abstract member RemoveNodes : 'T seq -> IPersistentMultiEdgeSet<'T>
        abstract member AddNeighbour : ('T * 'T) -> IPersistentMultiEdgeSet<'T>
        abstract member AddNeighbours : ('T * 'T) seq -> IPersistentMultiEdgeSet<'T>
        abstract member RemoveNeighbour : ('T * 'T) -> IPersistentMultiEdgeSet<'T>
        abstract member RemoveNeighbours : ('T * 'T) seq -> IPersistentMultiEdgeSet<'T>
        abstract member AddEdge : ('T * 'T * int) -> IPersistentMultiEdgeSet<'T>
        abstract member AddEdges : ('T * 'T * int) seq -> IPersistentMultiEdgeSet<'T>
        abstract member RemoveEdge : ('T * 'T * int) -> IPersistentMultiEdgeSet<'T>
        abstract member RemoveEdges : ('T * 'T * int) seq -> IPersistentMultiEdgeSet<'T>
        abstract member AsFrozen : unit -> IMultiEdgeSet<'T>

    type IPersistentMultiEdgeWithDataSet<'T, 'E> =
        inherit IMultiEdgeWithDataSet<'T, 'E>
        abstract member RemoveNode : 'T -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member RemoveNodes : 'T seq -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member AddNeighbour : ('T * 'T * 'E) -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member AddNeighbours : ('T * 'T * 'E) seq -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member RemoveNeighbour : ('T * 'T) -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member RemoveNeighbours : ('T * 'T) seq -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member AddEdge : ('T * 'T * int * 'E) -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member AddEdges : ('T * 'T * int * 'E) seq -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member RemoveEdge : ('T * 'T * int) -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member RemoveEdges : ('T * 'T * int) seq -> IPersistentMultiEdgeWithDataSet<'T, 'E>
        abstract member AsFrozen : unit -> IMultiEdgeWithDataSet<'T, 'E>

    type FrozenMultiEdgeWithDataSet<'T, 'E>(edges : Dictionary<'T, Dictionary<'T, Dictionary<int, 'E>>>) =
        interface IMultiEdgeWithDataSet<'T, 'E> with
            member __.Neighbours n = DictHelpers.Keys2 n edges
            member __.HasNeighbour neighbour = DictHelpers.ContainsKey2 neighbour edges
            member __.Edges n = DictHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> Seq.map (fun i -> (k, i)) d.Keys)
            member __.HasEdge e = DictHelpers.ContainsKey3 e edges
            member __.EdgeCount n =
                match DictHelpers.GetOrDefault2 n null edges with
                | null -> 0
                | d -> d.Count
            member __.NeighboursWithData n = DictHelpers.AsTupleSeq2 n edges |> Seq.map (fun (k, v) -> (k, upcast v))
            member __.GetNeighbourData n = upcast (DictHelpers.GetOrThrow2 n edges)
            member __.EdgesWithData n = DictHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> d |> Seq.map (fun kvp -> (k, kvp.Key, kvp.Value)))
            member __.GetEdgeData e = DictHelpers.GetOrThrow3 e edges
        static member private edgesToMultiEdges edges =
            edges
            |> Seq.groupBy (fun (u, v, _) -> (u, v))
            |> Seq.collect (fun ((u, v), s) -> Seq.mapi (fun i (_, _, d) -> (u, v, i, d)) s)
        static member ofUndirectedEdgeSeq edges =
            edges
            |> FrozenMultiEdgeWithDataSet<_, _>.edgesToMultiEdges
            |> FrozenMultiEdgeWithDataSet<_, _>.ofUndirectedSeq
        static member ofEdgeSeq (edges : ('a * 'a * 'b) seq) =
            edges
            |> FrozenMultiEdgeWithDataSet<_, _>.edgesToMultiEdges
            |> FrozenMultiEdgeWithDataSet<_, _>.ofSeq
        static member ofUndirectedSeq multiEdges =
            multiEdges
            |> Seq.filter (fun (a, b, _, _) -> a <> b)
            |> Seq.map (fun (a, b, i, d) -> (b, a, i, d))
            |> Seq.append multiEdges
            |> FrozenMultiEdgeWithDataSet<_, _>.ofSeq
        static member ofSeq (multiEdges : ('a * 'a * int * 'b) seq) = multiEdges |> DictHelpers.FromTupleSeq3 |> FrozenMultiEdgeWithDataSet<_, _>
        static member empty = new FrozenMultiEdgeWithDataSet<_, _>(new Dictionary<_,_>())

    type FrozenMultiEdgeSet<'T>(edges : Dictionary<'T, Dictionary<'T, Dictionary<int, unit>>>) =
        inherit FrozenMultiEdgeWithDataSet<'T, unit>(edges)
        interface IMultiEdgeSet<'T>
        static member private edgesToMultiEdges edges =
            edges
            |> Seq.groupBy id
            |> Seq.collect (fun ((u, v), s) -> Seq.mapi (fun i _ -> (u, v, i)) s)
        static member ofUndirectedEdgeSeq edges =
            edges
            |> FrozenMultiEdgeSet<_>.edgesToMultiEdges
            |> FrozenMultiEdgeSet<_>.ofUndirectedSeq
        static member ofEdgeSeq (edges : ('a * 'a) seq) =
            edges
            |> FrozenMultiEdgeSet<_>.edgesToMultiEdges
            |> FrozenMultiEdgeSet<_>.ofSeq
        static member ofUndirectedSeq multiEdges =
            multiEdges
            |> Seq.filter (fun (a, b, _) -> a <> b)
            |> Seq.map (fun (a, b, i) -> (b, a, i))
            |> Seq.append multiEdges
            |> FrozenMultiEdgeSet<'T>.ofSeq
        static member ofSeq (multiEdges : ('a * 'a * int) seq) =
            multiEdges
            |> Seq.map (fun (u, v, i) -> (u, v, i, ()))
            |> DictHelpers.FromTupleSeq3
            |> FrozenMultiEdgeWithDataSet<_, _>

    type MultiEdgeWithDataSet<'T, 'E when 'T : comparison>(edges : Map<'T, Map<'T, Map<int, 'E>>>) =
        member private __.RemoveNode node = MapHelpers.RemoveFromFirst2Keys node edges |> MultiEdgeWithDataSet<_,_>
        member private __.AddNeighbour neighbour =
            let u, v, d = neighbour
            let indexes = MapHelpers.Keys3 (u, v) edges
            let nextIndex = if Seq.isEmpty indexes then 0 else (Seq.max indexes) + 1
            MapHelpers.Add3 (u, v, nextIndex) d edges |> MultiEdgeWithDataSet<_,_>
        member private __.RemoveNeighbour neighbour = MapHelpers.Remove2 neighbour edges |> MultiEdgeWithDataSet<_,_>
        member private __.AddEdge edge =
            let u, v, i, d = edge
            MapHelpers.Add3 (u, v, i) d edges |> MultiEdgeWithDataSet<_,_>
        member private __.RemoveEdge edge = MapHelpers.Remove3 edge edges |> MultiEdgeWithDataSet<_,_>
        interface IPersistentMultiEdgeWithDataSet<'T, 'E> with
            member __.Neighbours n = MapHelpers.Keys2 n edges
            member __.HasNeighbour neighbour = MapHelpers.ContainsKey2 neighbour edges
            member __.Edges n = MapHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> d |> Map.toSeq |> Seq.map (fun (i, _) -> (k, i)))
            member __.HasEdge e = MapHelpers.ContainsKey3 e edges
            member __.EdgeCount n = (MapHelpers.GetOrDefault2 n Map.empty edges).Count
            member __.NeighboursWithData n = MapHelpers.AsTupleSeq2 n edges |> Seq.map (fun (k, v) -> (k, upcast v))
            member __.GetNeighbourData n = upcast (MapHelpers.GetOrThrow2 n edges)
            member __.EdgesWithData n = MapHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> d |> Seq.map (fun kvp -> (k, kvp.Key, kvp.Value)))
            member __.GetEdgeData e = MapHelpers.GetOrThrow3 e edges
            member es.RemoveNode node = upcast es.RemoveNode node
            member es.RemoveNodes nodes = upcast (Seq.fold (fun (es : MultiEdgeWithDataSet<'T, 'E>) n -> es.RemoveNode n) es nodes)
            member es.AddNeighbour neighbour = upcast es.AddNeighbour neighbour
            member es.AddNeighbours neighbours = upcast (Seq.fold (fun (es : MultiEdgeWithDataSet<'T, 'E>) n -> es.AddNeighbour n) es neighbours)
            member es.RemoveNeighbour neighbour = upcast es.RemoveNeighbour neighbour
            member es.RemoveNeighbours neighbours = upcast (Seq.fold (fun (es : MultiEdgeWithDataSet<'T, 'E>) n -> es.RemoveNeighbour n) es neighbours)
            member es.AddEdge edge = upcast es.AddEdge edge
            member es.AddEdges edges = upcast (Seq.fold (fun (es : MultiEdgeWithDataSet<_, _>) n -> es.AddEdge n) es edges)
            member es.RemoveEdge edge = upcast es.RemoveEdge edge
            member es.RemoveEdges edges = upcast (Seq.fold (fun (es : MultiEdgeWithDataSet<_, _>) n -> es.RemoveEdge n) es edges)
            member __.AsFrozen () = upcast (MapHelpers.Flatten3 edges |> FrozenMultiEdgeWithDataSet<_, _>.ofSeq)
        static member private edgesToMultiEdges edges =
            edges
            |> Seq.groupBy id
            |> Seq.collect (fun ((u, v, d), s) -> Seq.mapi (fun i _ -> (u, v, i, d)) s)
        static member createU edges =
            edges
            |> MultiEdgeWithDataSet<_, _>.edgesToMultiEdges
            |> MultiEdgeWithDataSet<_, _>.createU
        static member create (edges : ('a * 'a * 'b) seq) =
            edges
            |> MultiEdgeWithDataSet<_, _>.edgesToMultiEdges
            |> MultiEdgeWithDataSet<_, _>.create
        static member createU multiEdges =
            multiEdges
            |> Seq.filter (fun (a, b, _, _) -> a <> b)
            |> Seq.map (fun (a, b, i, d) -> (b, a, i, d))
            |> Seq.append multiEdges
            |> MultiEdgeWithDataSet<_, _>.create
        static member create (multiEdges : ('a * 'a * int * 'b) seq) = multiEdges |> MapHelpers.FromTupleSeq3 |> MultiEdgeWithDataSet<_, _>
        static member empty = new MultiEdgeWithDataSet<_, _>(Map.empty)

    type MultiEdgeSet<'T when 'T : comparison>(edges : Map<'T, Map<'T, Map<int, unit>>>) =
        member private __.RemoveNode node = MapHelpers.RemoveFromFirst2Keys node edges |> MultiEdgeSet<_>
        member private __.AddNeighbour neighbour =
            let u, v = neighbour
            let indexes = MapHelpers.Keys3 (u, v) edges
            let nextIndex = if Seq.isEmpty indexes then 0 else (Seq.max indexes) + 1
            MapHelpers.Add3 (u, v, nextIndex) () edges |> MultiEdgeSet<_>
        member private __.RemoveNeighbour neighbour = MapHelpers.Remove2 neighbour edges |> MultiEdgeSet<_>
        member private __.AddEdge edge =
            let u, v, i = edge
            MapHelpers.Add3 (u, v, i) () edges |> MultiEdgeSet<_>
        member private __.RemoveEdge edge = MapHelpers.Remove3 edge edges |> MultiEdgeSet<_>
        interface IPersistentMultiEdgeSet<'T> with
            member __.Neighbours n = MapHelpers.Keys2 n edges
            member __.HasNeighbour neighbour = MapHelpers.ContainsKey2 neighbour edges
            member __.Edges n = MapHelpers.AsTupleSeq2 n edges |> Seq.collect (fun (k, d) -> d |> Map.toSeq |> Seq.map (fun (i, _) -> (k, i)))
            member __.HasEdge e = MapHelpers.ContainsKey3 e edges
            member __.EdgeCount n = (MapHelpers.GetOrDefault2 n Map.empty edges).Count
            member es.RemoveNode node = upcast es.RemoveNode node
            member es.RemoveNodes nodes = upcast (Seq.fold (fun (es : MultiEdgeSet<_>) n -> es.RemoveNode n) es nodes)
            member es.AddNeighbour neighbour = upcast es.AddNeighbour neighbour
            member es.AddNeighbours neighbours = upcast (Seq.fold (fun (es : MultiEdgeSet<_>) n -> es.AddNeighbour n) es neighbours)
            member es.RemoveNeighbour neighbour = upcast es.RemoveNeighbour neighbour
            member es.RemoveNeighbours neighbours = upcast (Seq.fold (fun (es : MultiEdgeSet<_>) n -> es.RemoveNeighbour n) es neighbours)
            member es.AddEdge edge = upcast es.AddEdge edge
            member es.AddEdges edges = upcast (Seq.fold (fun (es : MultiEdgeSet<_>) n -> es.AddEdge n) es edges)
            member es.RemoveEdge edge = upcast es.RemoveEdge edge
            member es.RemoveEdges edges = upcast (Seq.fold (fun (es : MultiEdgeSet<_>) n -> es.RemoveEdge n) es edges)
            member __.AsFrozen () = upcast (MapHelpers.Flatten3 edges |> Seq.map (fun (u, v, i, _) -> (u, v, i))  |> FrozenMultiEdgeSet<_>.ofSeq)
        static member private edgesToMultiEdges edges =
            edges
            |> Seq.groupBy id
            |> Seq.collect (fun ((u, v), s) -> Seq.mapi (fun i _ -> (u, v, i)) s)
        static member createU edges =
            edges
            |> MultiEdgeSet<_>.edgesToMultiEdges
            |> MultiEdgeSet<_>.createU
        static member create (edges : ('a * 'a) seq) =
            edges
            |> MultiEdgeSet<_>.edgesToMultiEdges
            |> MultiEdgeSet<_>.create
        static member createU multiEdges =
            multiEdges
            |> Seq.filter (fun (a, b, _) -> a <> b)
            |> Seq.map (fun (a, b, i) -> (b, a, i))
            |> Seq.append multiEdges
            |> MultiEdgeSet<_>.create
        static member create (multiEdges : ('a * 'a * int) seq) = 
            multiEdges
            |> Seq.map (fun (u, v, i) -> (u, v, i, ()))
            |> MapHelpers.FromTupleSeq3
            |> MultiEdgeSet<_>
        static member empty = new MultiEdgeSet<_>(Map.empty)

module Graph =
    open NodeSet
    open EdgeSet
    open MultiEdgeSet
    
    let inline hasNode n (N : INodeSet<'N>, _) = N.HasNode n
    let inline nodes (N: IFiniteNodeSet<'N>, _) = N.Nodes

    let inline addNode n (N, E) =
        let newNodes = (^NS : (member AddNode : 'N -> ^NS) (N, n))
        newNodes, E

    let inline addNodes ns (N, E) =
        let newNodes = (^NS : (member AddNodes : 'N seq -> ^NS) (N, ns))
        newNodes, E

    let inline removeNode node (N, E) =
        let newNodes = (^NS : (member RemoveNode : 'N -> ^NS) (N, node))
        let newEdges = (^ES : (member RemoveNode : 'N -> ^ES) (E, node))
        newNodes, newEdges

    let inline removeNodes nodes (N, E) =
        let newNodes = (^NS : (member RemoveNodes : 'N seq -> ^NS) (N, nodes))
        let newEdges = (^ES : (member RemoveNodes : 'N seq -> ^ES) (E, nodes))
        newNodes, newEdges

    let inline addEdge (u, v) (N, E) =
        let newNodes = (^NS : (member AddNodes : 'N seq -> ^NS) (N, [|u; v|]))
        let newEdges = (^ES : (member AddNeighbour : ('N * 'N) -> ^ES) (E, (u, v)))
        newNodes, newEdges

    let inline addEdges edges (N, E) =
        let allNodes = edges |> Seq.collect (fun (u, v) -> [| u; v |])
        let newNodes = (^NS : (member AddNodes : 'N seq -> ^NS) (N, allNodes))
        let newEdges = (^ES : (member AddNeighbours : ('N * 'N) seq -> ^ES) (E, edges))
        newNodes, newEdges

    let inline addEdgeWithData (u, v, d) (N, E) =
        let newNodes = (^NS : (member AddNodes : 'N seq -> ^NS) (N, [|u; v|]))
        let newEdges = (^ES : (member AddNeighbour : ('N * 'N * 'E) -> ^ES) (E, (u, v, d)))
        newNodes, newEdges

    let inline addEdgesWithData edges (N, E) =
        let allNodes = edges |> Seq.collect (fun (u, v, _) -> [| u; v |])
        let newNodes = (^NS : (member AddNodes : 'N seq -> ^NS) (N, allNodes))
        let newEdges = (^ES : (member AddNeighbours : ('N * 'N * 'E) seq -> ^ES) (E, edges))
        newNodes, newEdges

    let inline addMultiEdge (u, v, i) (N, E) =
        let newNodes = (^NS : (member AddNodes : 'N seq -> ^NS) (N, [|u; v|]))
        let newEdges = (^ES : (member AddEdge : ('N * 'N * int) -> ^ES) (E, (u, v, i)))
        newNodes, newEdges

    let inline addMultiEdges multiEdges (N, E) =
        let allNodes = multiEdges |> Seq.collect (fun (u, v, _) -> [| u; v |])
        let newNodes = (^NS : (member AddNodes : 'N seq -> ^NS) (N, allNodes))
        let newEdges = (^ES : (member AddEdges : ('N * 'N * int) seq -> ^ES) (E, multiEdges))
        newNodes, newEdges

    let inline addMultiEdgeWithData (u, v, i, d) (N, E) =
        let newNodes = (^NS : (member AddNodes : 'N seq -> ^NS) (N, [|u; v|]))
        let newEdges = (^ES : (member AddEdge : ('N * 'N * int * 'E) -> ^ES) (E, (u, v, i, d)))
        newNodes, newEdges

    let inline addMultiEdgesWithData multiEdges (N, E) =
        let allNodes = multiEdges |> Seq.collect (fun (u, v, _, _) -> [| u; v |])
        let newNodes = (^NS : (member AddNodes : 'N seq -> ^NS) (N, allNodes))
        let newEdges = (^ES : (member AddEdges : ('N * 'N * int * 'E) seq -> ^ES) (E, multiEdges))
        newNodes, newEdges

    let inline removeEdge edge (N, E) =
        let newEdges = (^ES : (member RemoveNeighbour : ('N * 'N) -> ^ES) (E, edge))
        N, newEdges

    let inline removeEdges edges (N, E) =
        let newEdges = (^ES : (member RemoveNeighbours : ('N * 'N) seq -> ^ES) (E, edges))
        N, newEdges

    let inline removeMultiEdge multiEdge (N, E) =
        let newEdges = (^ES : (member RemoveEdge : ('N * 'N * int) -> ^ES) (E, multiEdge))
        N, newEdges

    let inline removeMultiEdges multiEdges (N, E) =
        let newEdges = (^ES : (member RemoveEdges : ('N * 'N * int) seq -> ^ES) (E, multiEdges))
        N, newEdges

    let inline freeze (N, E) =
        let newNodes = (^NS : (member AsFrozen : unit -> ^FNS) N)
        let newEdges = (^ES : (member AsFrozen : unit -> ^FES) E)
        newNodes, newEdges

    let inline neighboursWithData n (_, E : IEdgeWithDataSet<'N, 'E>) = E.NeighboursWithData n
    let inline getEdgeData edge (_, E : IEdgeWithDataSet<'N, 'E>) = E.GetNeighbourData edge
    let inline multiEdgesWithData multiEdge (_, E : IMultiEdgeWithDataSet<'N, 'E>) = E.EdgesWithData multiEdge
    let inline getMultiEdgeData multiEdge (_, E : IMultiEdgeWithDataSet<'N, 'E>) = E.GetEdgeData multiEdge

    let inline neighbours n (_, E : IEdgeSet<'N>) = E.Neighbours n
    let inline hasEdge e (_, E : IEdgeSet<'N>) = E.HasNeighbour e
    let inline hasMultiEdge e (_, E : IMultiEdgeSet<'N>) = E.HasEdge e

