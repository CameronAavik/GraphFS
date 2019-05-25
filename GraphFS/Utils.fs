namespace GraphFs

open System.Collections.Generic

module Utils =
    type ImmutableDictionary<'K, 'V>(d: IDictionary<'K, 'V>) =
        interface IEnumerable<KeyValuePair<'K, 'V>> with
            member __.GetEnumerator() = d.GetEnumerator()

        interface System.Collections.IEnumerable with
            member __.GetEnumerator() = upcast d.GetEnumerator()

        interface IReadOnlyCollection<KeyValuePair<'K, 'V>> with
            member __.Count = d.Count

        interface IReadOnlyDictionary<'K, 'V> with
            member __.Item with get(k) = d.[k]
            member __.Keys = seq d.Keys
            member __.Values = seq d.Values
            member __.ContainsKey k = d.ContainsKey k
            member __.TryGetValue(k, value) = d.TryGetValue(k, &value)

    type DoubleImmutableDictionary<'K1, 'K2, 'V>(d: IDictionary<'K1, IDictionary<'K2, 'V>>) =
        member private __.AsSeq = Seq.map (fun k -> new KeyValuePair<'K1, IReadOnlyDictionary<'K2, 'V>>(k, ImmutableDictionary(d.[k]))) d.Keys

        interface IEnumerable<KeyValuePair<'K1, IReadOnlyDictionary<'K2, 'V>>> with
            member did.GetEnumerator() = did.AsSeq.GetEnumerator()

        interface System.Collections.IEnumerable with
            member did.GetEnumerator() = upcast did.AsSeq.GetEnumerator()

        interface IReadOnlyCollection<KeyValuePair<'K1, IReadOnlyDictionary<'K2, 'V>>> with
            member __.Count = d.Count

        interface IReadOnlyDictionary<'K1, IReadOnlyDictionary<'K2, 'V>> with
            member __.Item with get(k) = upcast ImmutableDictionary(d.[k])
            member __.Keys = seq d.Keys
            member __.Values = d.Values |> Seq.map (fun v -> upcast ImmutableDictionary(v))
            member __.ContainsKey k = d.ContainsKey k
            member __.TryGetValue(k, value) =
                if d.ContainsKey k then
                    value <- ImmutableDictionary(d.[k])
                    true
                else false
