namespace GraphFS.Helpers

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