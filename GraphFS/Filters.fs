namespace GraphFs

open System.Collections.Generic

module Filters =
    type Filter<'a> = 'a -> bool

    let internal hide<'T> (items : 'T seq) = 
        let items = new HashSet<'T>(items)
        fun item -> items.Contains item |> not

    let internal show<'T> (items : 'T seq) = 
        let items = new HashSet<'T>(items)
        fun item -> items.Contains item

    let internal reverseEdge (u, v) = (v, u)
    let internal reverseMultiEdge (u, v, k) = (v, u, k)

    let internal toDirected reverseF edges =
        let reverseEdges = Seq.map reverseF edges
        Seq.append edges reverseEdges

    let internal toDiEdges edges = toDirected reverseEdge edges
    let internal toMultiDiEdges edges = toDirected reverseMultiEdge edges

    let noFilter : Filter<'a> = fun _ -> true
        
    let hideNodes nodes : Filter<'a> = hide nodes
    let hideDiEdges edges : Filter<'a * 'b> = hide edges
    let hideMultiDiEdges edges : Filter<'a * 'b * 'c>  = hide edges
    let hideEdges edges : Filter<'a * 'a> = edges |> toDiEdges |> hideDiEdges
    let hideMultiEdges edges : Filter<'a * 'a * 'b> = edges |> toMultiDiEdges |> hideMultiDiEdges

    let showNodes nodes : Filter<'a> = show nodes
    let showDiEdges edges : Filter<'a * 'b> = show edges
    let showMultiDiEdges edges : Filter<'a * 'b * 'c> = show edges
    let showEdges edges : Filter<'a * 'a> = edges |> toDiEdges |> showDiEdges
    let showMultiEdges edges : Filter<'a * 'a * 'b> = edges |> toMultiDiEdges |> showMultiDiEdges