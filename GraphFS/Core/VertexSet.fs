namespace GraphFS.Core

open System.Collections.Generic

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

