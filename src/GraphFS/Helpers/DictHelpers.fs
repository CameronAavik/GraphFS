namespace GraphFS.Helpers

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
        let d = new Dictionary<'K1, Dictionary<'K2, 'V>>()
        for (u, v, data) in edges do
            Add2 (u, v) data d
        d

    let inline FromTupleSeq3 edges =
        let d = new Dictionary<'K1, Dictionary<'K2, Dictionary<'K3, 'V>>>()
        for (u, v, i, data) in edges do
            Add3 (u, v, i) data d
        d