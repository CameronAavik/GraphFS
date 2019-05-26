namespace GraphFs

open Filters
open System.Collections.Generic
open System.Linq
open System

module CoreViews =
    type IRODict<'K, 'V> = IReadOnlyDictionary<'K, 'V>
    type IRODict2<'K1, 'K2, 'V> = IRODict<'K1, IRODict<'K2, 'V>>
    type IRODict3<'K1, 'K2, 'K3, 'V> = IRODict<'K1, IRODict2<'K2, 'K3, 'V>>

    type EmptyAtlas<'K, 'V>() =
        interface IEnumerable<KeyValuePair<'K, 'V>> with
            member __.GetEnumerator() = Enumerable.Empty().GetEnumerator()

        interface System.Collections.IEnumerable with
            member __.GetEnumerator() = upcast Enumerable.Empty().GetEnumerator()

        interface IReadOnlyCollection<KeyValuePair<'K, 'V>> with
            member __.Count = 0

        interface IRODict<'K, 'V> with
            member __.Item 
                with get(k) = 
                    if obj.ReferenceEquals(k, null) then raise (ArgumentNullException())
                    else raise (KeyNotFoundException())
            member __.Keys = Seq.empty
            member __.Values = Seq.empty
            member __.ContainsKey _ = false
            member __.TryGetValue(_, value) = false

    type AtlasView<'K, 'V>(d : IRODict<'K, 'V>) =
        abstract Item : 'K -> 'V with get
        abstract Keys : 'K seq
        abstract ContainsKey : 'K -> bool
        abstract Count : int

        default __.Item with get(k) = d.[k]
        default __.Keys = d.Keys
        default __.ContainsKey k = d.ContainsKey k
        default __.Count = d.Count

        member private av.AsSeq = Seq.map (fun k -> new KeyValuePair<'K, 'V>(k, av.[k])) av.Keys

        interface IRODict<'K, 'V> with
            member av.Item with get(k) = av.[k]
            member av.Keys = av.Keys
            member av.Values = seq { for k in av.Keys -> av.[k] }
            member av.ContainsKey k = av.ContainsKey k
            member av.TryGetValue(k, value) = 
                if av.ContainsKey k then
                    value <- av.[k]
                    true
                else false
            member av.Count = av.Count
            member av.GetEnumerator() = av.AsSeq.GetEnumerator() :> Collections.IEnumerator
            member av.GetEnumerator() = av.AsSeq.GetEnumerator()

    type AdjacencyView<'K1, 'K2, 'V>(d : IRODict2<'K1,'K2, 'V>) =
        inherit AtlasView<'K1, IRODict<'K2, 'V>>(d)
        override __.Item with get(k) = upcast AtlasView(d.[k])
        interface IRODict2<'K1,'K2, 'V>

    type MultiAdjacencyView<'K1, 'K2, 'K3, 'V>(d : IRODict3<'K1, 'K2, 'K3, 'V>) =
        inherit AdjacencyView<'K1, 'K2, IRODict<'K3, 'V>>(d)
        override __.Item with get(k) = upcast AdjacencyView(d.[k])
        interface IRODict3<'K1, 'K2, 'K3, 'V>

    type UnionAtlas<'K, 'V>(succ: IRODict<'K, 'V>, pred: IRODict<'K, 'V>) =
        abstract Item : 'K -> 'V with get
        abstract Keys : 'K seq
        abstract ContainsKey : 'K -> bool
        abstract Count : int

        default __.Item with get(k) = if succ.ContainsKey k then succ.[k] else pred.[k]
        default __.Keys = 
            let seen = new HashSet<'K>()
            seq { for dict in [succ; pred] do
                    for k in dict.Keys do
                        if not (seen.Contains k) then
                            seen.Add(k) |> ignore
                            yield k }
        default __.ContainsKey k = succ.ContainsKey k || pred.ContainsKey k
        default ua.Count = ua.Keys.Count()

        member private ua.AsSeq = Seq.map (fun k -> new KeyValuePair<'K, 'V>(k, ua.[k])) ua.Keys
        
        interface IRODict<'K, 'V> with
            member ua.Item with get(k) = ua.[k]
            member ua.Keys = ua.Keys
            member ua.Values = seq { for k in ua.Keys -> ua.[k] }
            member ua.ContainsKey k = ua.ContainsKey k
            member ua.TryGetValue(k, value) = 
                if ua.ContainsKey k then
                    value <- ua.[k]
                    true
                else false
            member ua.Count = ua.Count
            member ua.GetEnumerator() = ua.AsSeq.GetEnumerator() :> Collections.IEnumerator
            member ua.GetEnumerator() = ua.AsSeq.GetEnumerator()

    type UnionAdjacency<'K1, 'K2, 'V>(succ: IRODict2<'K1, 'K2, 'V>, pred: IRODict2<'K1, 'K2, 'V>) =
        inherit UnionAtlas<'K1, IRODict<'K2, 'V>>(succ, pred)
        override __.Item with get(k) = upcast UnionAtlas(succ.[k], pred.[k])
        override __.Keys = succ.Keys
        override __.ContainsKey k = succ.ContainsKey k
        override __.Count = succ.Count
        interface IRODict2<'K1, 'K2, 'V>

    type UnionMultiInner<'K1, 'K2, 'V>(succ: IRODict2<'K1, 'K2, 'V>, pred: IRODict2<'K1, 'K2, 'V>) =
        inherit UnionAtlas<'K1, IRODict<'K2, 'V>>(succ, pred)
        override __.Item
            with get(k) =
                let in_succ = succ.ContainsKey k
                let in_pred = pred.ContainsKey k
                if in_succ && in_pred then
                    upcast UnionAtlas(succ.[k], pred.[k])
                else
                    let emptyAtlas = EmptyAtlas<'K2, 'V>()
                    let succVal = if in_succ then succ.[k] else upcast emptyAtlas
                    let predVal = if in_pred then pred.[k] else upcast emptyAtlas
                    upcast UnionAtlas(succVal, predVal)
        interface IRODict2<'K1, 'K2, 'V>

    type UnionMultiAdjacency<'K1, 'K2, 'K3, 'V>(succ: IRODict3<'K1, 'K2, 'K3, 'V>, pred: IRODict3<'K1, 'K2, 'K3, 'V>) =
        inherit UnionAdjacency<'K1, 'K2, IRODict<'K3, 'V>>(succ, pred)
        override __.Item  with get(k) = upcast UnionMultiInner(succ.[k], pred.[k])
        interface IRODict3<'K1, 'K2, 'K3, 'V>

    type FilterAtlas<'K, 'V>(atlas: IRODict<'K, 'V>, isNodeOk: Filter<'K>) =
        inherit AtlasView<'K, 'V>(atlas)

        member private __.OkNodes = atlas.Keys |> Seq.filter isNodeOk

        override __.Item with get(k) = if isNodeOk k then atlas.[k] else raise (KeyNotFoundException())
        override fa.Keys = fa.OkNodes
        override __.ContainsKey k = isNodeOk k && atlas.ContainsKey k
        override fa.Count = fa.OkNodes |> Seq.length
        interface IRODict<'K, 'V>

    type FilterAdjacency<'K, 'V>(atlas: IRODict2<'K, 'K, 'V>, isNodeOk: Filter<'K>, isEdgeOk: Filter<'K * 'K>) =
        inherit FilterAtlas<'K, IRODict<'K, 'V>>(atlas, isNodeOk)
        override __.Item
            with get(k) = 
                if isNodeOk k then
                    let newNodeOk node = isNodeOk node && isEdgeOk (k, node)
                    upcast FilterAtlas(atlas.[k], newNodeOk) 
                else raise (KeyNotFoundException())
        interface IRODict2<'K, 'K, 'V>

    type FilterMultiInner<'K1, 'K2, 'V>(atlas: IRODict2<'K1, 'K2, 'V>, isNodeOk: Filters.Filter<'K1>, isEdgeOk: Filters.Filter<'K1 * 'K2>) =
        inherit FilterAtlas<'K1, IRODict<'K2, 'V>>(atlas, isNodeOk)

        member private __.OkNodesWithOkEdges =
            let nodeHasOkEdge n = Seq.exists (fun t -> isEdgeOk (n, t)) atlas.[n].Keys
            atlas.Keys
            |> Seq.filter isNodeOk
            |> Seq.filter nodeHasOkEdge

        override __.Item
            with get(k) = 
                if isNodeOk k then
                    let newNodeOk node = isEdgeOk (k, node)
                    upcast FilterAtlas(atlas.[k], newNodeOk) 
                else raise (KeyNotFoundException())
        override fmi.Keys = fmi.OkNodesWithOkEdges
        override fmi.Count = fmi.OkNodesWithOkEdges |> Seq.length
        interface IRODict2<'K1, 'K2, 'V>

    type FilterMultiAdjacency<'K1, 'K2, 'V>(atlas: IRODict3<'K1, 'K1, 'K2, 'V>, isNodeOk: Filters.Filter<'K1>, isEdgeOk: Filters.Filter<'K1 * 'K1 * 'K2>) =
        inherit FilterAtlas<'K1, IRODict2<'K1, 'K2, 'V>>(atlas, isNodeOk)
        override __.Item
            with get(k) = 
                if isNodeOk k then
                    let newEdgeOk (node, key) = isEdgeOk (k, node, key)
                    upcast FilterMultiInner(atlas.[k], isNodeOk, newEdgeOk) 
                else raise (KeyNotFoundException())
        interface IRODict3<'K1, 'K1, 'K2, 'V>