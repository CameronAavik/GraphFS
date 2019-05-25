namespace GraphFs

open System.Collections.Generic
open CoreViews

module Graph =
    type Node<'T, 'Data> =
        | Node of 'T
        | NodeWithData of 'T * 'Data

    type EdgeNodes<'T> = 'T * 'T

    type Edge<'T, 'Data> =
        | Edge of EdgeNodes<'T>
        | EdgeWithData of EdgeNodes<'T> * 'Data

    type MultiEdge<'T> = 'T * 'T * int

    type MultiGraphEdgeData<'T> = AtlasView<int, 'T>
    
    type IGraph<'Node, 'NodeData, 'EdgeData> =
        abstract member Adj : AdjacencyView<'Node, 'Node, 'EdgeData>
        abstract member Item : 'Node -> AtlasView<'Node, 'EdgeData> with get
        abstract member Item : EdgeNodes<'Node> -> 'EdgeData with get
        abstract member Nodes : 'Node seq
        abstract member GetNodeData : 'Node -> 'NodeData
        abstract member GetEdgeData : EdgeNodes<'Node> -> 'EdgeData
        abstract member Neighbours : 'Node -> 'Node seq
        abstract member NumberOfNodes : int
        abstract member HasNode : 'Node -> bool
        abstract member HasEdge : EdgeNodes<'Node> -> bool
        abstract member NewNodeData : 'Node -> 'NodeData
        abstract member NewEdgeData : EdgeNodes<'Node>  -> 'EdgeData
        abstract member IsDirected : bool
        abstract member SupportsMultipleParallelEdges : bool
        abstract member SupportsSelfLoops : bool
        inherit IRODict2<'Node, 'Node, 'EdgeData>

    type IMutableGraph<'Node, 'NodeData, 'EdgeData> =
        inherit IGraph<'Node, 'NodeData, 'EdgeData>
        abstract member AddNode : Node<'Node, 'NodeData> -> unit
        abstract member AddNodesFrom : Node<'Node, 'NodeData> seq -> unit
        abstract member RemoveNode : 'Node -> unit
        abstract member RemoveNodesFrom : 'Node seq -> unit
        abstract member AddEdge : Edge<'Node, 'EdgeData> -> unit
        abstract member AddEdgesFrom : Edge<'Node, 'EdgeData> seq -> unit
        abstract member RemoveEdge : EdgeNodes<'Node> -> unit
        abstract member RemoveEdgesFrom : EdgeNodes<'Node> seq -> unit
        abstract member UpdateFrom : IGraph<'Node, 'NodeData, 'EdgeData> -> unit

    type IImmutableGraph<'Node, 'NodeData, 'EdgeData> =
        inherit IGraph<'Node, 'NodeData, 'EdgeData>
        abstract member AddNode : Node<'Node, 'NodeData> -> IImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member AddNodesFrom : Node<'Node, 'NodeData> seq -> IImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveNode : 'Node -> IImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveNodesFrom : 'Node seq -> IImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member AddEdge : Edge<'Node, 'EdgeData> -> IImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member AddEdgesFrom : Edge<'Node, 'EdgeData> seq -> IImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveEdge : EdgeNodes<'Node> -> IImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveEdgesFrom : EdgeNodes<'Node> seq -> IImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member MergeWith : IGraph<'Node, 'NodeData, 'EdgeData> -> IImmutableGraph<'Node, 'NodeData, 'EdgeData>

    type IMultiGraph<'Node, 'NodeData, 'EdgeData> =
        inherit IGraph<'Node, 'NodeData, MultiGraphEdgeData<'EdgeData>>
        abstract member Item : MultiEdge<'Node> -> 'EdgeData with get

    type IMutableMultiGraph<'Node, 'NodeData, 'EdgeData> =
        inherit IMultiGraph<'Node, 'NodeData, 'EdgeData>
        inherit IMutableGraph<'Node, 'NodeData, MultiGraphEdgeData<'EdgeData>>
        abstract member RemoveMultiEdge : MultiEdge<'Node> -> unit
        abstract member RemoveMultiEdgesFrom : MultiEdge<'Node> seq -> unit

    type IImmutableMultiGraph<'Node, 'NodeData, 'EdgeData> =
        inherit IMultiGraph<'Node, 'NodeData, 'EdgeData>
        inherit IImmutableGraph<'Node, 'NodeData, MultiGraphEdgeData<'EdgeData>>
        abstract member RemoveMultiEdge : MultiEdge<'Node> -> IImmutableMultiGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveMultiEdgesFrom : MultiEdge<'Node> seq -> IImmutableMultiGraph<'Node, 'NodeData, 'EdgeData>

    [<AbstractClass>]
    type Graph<'Node, 'NodeData, 'EdgeData>() =
        abstract member Adj : AdjacencyView<'Node, 'Node, 'EdgeData>
        abstract member Item : 'Node -> AtlasView<'Node, 'EdgeData> with get
        abstract member Item : EdgeNodes<'Node> -> 'EdgeData with get
        abstract member Nodes : 'Node seq
        abstract member GetNodeData : 'Node -> 'NodeData
        abstract member GetEdgeData : EdgeNodes<'Node> -> 'EdgeData
        abstract member Neighbours : 'Node -> 'Node seq
        abstract member NumberOfNodes : int
        abstract member HasNode : 'Node -> bool
        abstract member HasEdge : EdgeNodes<'Node> -> bool
        abstract member NewNodeData : 'Node -> 'NodeData
        abstract member NewEdgeData : EdgeNodes<'Node>  -> 'EdgeData
        abstract member IsDirected : bool
        abstract member SupportsMultipleParallelEdges : bool
        abstract member SupportsSelfLoops : bool

        abstract member AsEnumerable : unit -> IEnumerable<KeyValuePair<'Node, IRODict<'Node, 'EdgeData>>>

        interface IGraph<'Node, 'NodeData, 'EdgeData> with
            member g.Adj = g.Adj
            member g.Item with get(k: 'Node) = g.[k]
            member g.Item with get(k: EdgeNodes<'Node>) = g.[k]
            member g.Nodes = g.Nodes
            member g.GetNodeData n = g.GetNodeData n
            member g.GetEdgeData e = g.GetEdgeData e
            member g.Neighbours n = g.Neighbours n
            member g.NumberOfNodes = g.NumberOfNodes
            member g.HasNode n = g.HasNode n
            member g.HasEdge e = g.HasEdge e
            member g.NewNodeData n = g.NewNodeData n
            member g.NewEdgeData e = g.NewEdgeData e
            member g.IsDirected = g.IsDirected
            member g.SupportsMultipleParallelEdges = g.SupportsMultipleParallelEdges
            member g.SupportsSelfLoops  = g.SupportsSelfLoops

        interface IEnumerable<KeyValuePair<'Node, IRODict<'Node, 'EdgeData>>> with
            member g.GetEnumerator() = g.AsEnumerable().GetEnumerator()
        
        interface System.Collections.IEnumerable with
            member g.GetEnumerator() = upcast g.AsEnumerable().GetEnumerator()

        interface IReadOnlyCollection<KeyValuePair<'Node, IRODict<'Node, 'EdgeData>>> with
            member g.Count = g.NumberOfNodes
        
        interface IRODict2<'Node, 'Node, 'EdgeData> with
            member g.Item with get(k : 'Node) = upcast g.[k]
            member g.Keys = g.Nodes
            member g.Values = seq { for k in g.Nodes -> g.[k] }
            member g.ContainsKey k = g.HasNode k
            member g.TryGetValue(k, value) =
                if g.HasNode k then value <- g.[k]; true
                else false

    [<AbstractClass>]
    type MutableGraph<'Node, 'NodeData, 'EdgeData>() =
        inherit Graph<'Node, 'NodeData, 'EdgeData>()

        abstract member AddNode : Node<'Node, 'NodeData> -> unit
        abstract member AddNodesFrom : Node<'Node, 'NodeData> seq -> unit
        abstract member RemoveNode : 'Node -> unit
        abstract member RemoveNodesFrom : 'Node seq -> unit
        abstract member AddEdge : Edge<'Node, 'EdgeData> -> unit
        abstract member AddEdgesFrom : Edge<'Node, 'EdgeData> seq -> unit
        abstract member RemoveEdge : EdgeNodes<'Node> -> unit
        abstract member RemoveEdgesFrom : EdgeNodes<'Node> seq -> unit
        abstract member UpdateFrom : IGraph<'Node, 'NodeData, 'EdgeData> -> unit

        default g.AddNodesFrom nodes = Seq.iter g.AddNode nodes
        default g.RemoveNodesFrom nodes = Seq.iter g.RemoveNode nodes
        default g.AddEdgesFrom edges = Seq.iter g.AddEdge edges
        default g.RemoveEdgesFrom edges = Seq.iter g.RemoveEdge edges
        default g.UpdateFrom graph =
            let getEdgesFromNode (node : 'Node) = seq { for kvp in graph.[node] -> EdgeWithData ((node, kvp.Key), kvp.Value) }
            graph.Nodes |> Seq.map (fun n -> NodeWithData (n, graph.GetNodeData n)) |> g.AddNodesFrom
            graph.Nodes |> Seq.collect getEdgesFromNode |> g.AddEdgesFrom

        interface IMutableGraph<'Node, 'NodeData, 'EdgeData> with
            member g.AddNode n = g.AddNode n
            member g.AddNodesFrom ns = g.AddNodesFrom ns
            member g.RemoveNode n = g.RemoveNode n
            member g.RemoveNodesFrom ns = g.RemoveNodesFrom ns
            member g.AddEdge e = g.AddEdge e
            member g.AddEdgesFrom es = g.AddEdgesFrom es
            member g.RemoveEdge e = g.RemoveEdge e
            member g.RemoveEdgesFrom es = g.RemoveEdgesFrom es
            member g.UpdateFrom graph = g.UpdateFrom graph

    [<AbstractClass>]
    type ImmutableGraph<'Node, 'NodeData, 'EdgeData>() =
        inherit Graph<'Node, 'NodeData, 'EdgeData>()

        abstract member AddNode : Node<'Node, 'NodeData> -> ImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member AddNodesFrom : Node<'Node, 'NodeData> seq -> ImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveNode : 'Node -> ImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveNodesFrom : 'Node seq -> ImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member AddEdge : Edge<'Node, 'EdgeData> -> ImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member AddEdgesFrom : Edge<'Node, 'EdgeData> seq -> ImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveEdge : EdgeNodes<'Node> -> ImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveEdgesFrom : EdgeNodes<'Node> seq -> ImmutableGraph<'Node, 'NodeData, 'EdgeData>
        abstract member MergeWith : IGraph<'Node, 'NodeData, 'EdgeData> -> ImmutableGraph<'Node, 'NodeData, 'EdgeData>

        default g.AddNodesFrom nodes = Seq.fold (fun g n -> g.AddNode n) g nodes
        default g.RemoveNodesFrom nodes = Seq.fold (fun g n -> g.RemoveNode n) g nodes
        default g.AddEdgesFrom edges = Seq.fold (fun g e -> g.AddEdge e) g edges
        default g.RemoveEdgesFrom edges = Seq.fold (fun g e -> g.RemoveEdge e) g edges
        default g.MergeWith graph =
            let getEdgesFromNode (node : 'Node) = seq { for kvp in graph.[node] -> EdgeWithData ((node, kvp.Key), kvp.Value) }
            let nodesToAdd = graph.Nodes |> Seq.map (fun n -> NodeWithData (n, graph.GetNodeData n))
            let edgesToAdd = graph.Nodes |> Seq.collect getEdgesFromNode
            let g = g.AddNodesFrom nodesToAdd
            g.AddEdgesFrom edgesToAdd

        interface IImmutableGraph<'Node, 'NodeData, 'EdgeData> with
            member g.AddNode n = upcast g.AddNode n
            member g.AddNodesFrom ns = upcast g.AddNodesFrom ns
            member g.RemoveNode n = upcast g.RemoveNode n
            member g.RemoveNodesFrom ns = upcast g.RemoveNodesFrom ns
            member g.AddEdge e = upcast g.AddEdge e
            member g.AddEdgesFrom es = upcast g.AddEdgesFrom es
            member g.RemoveEdge e = upcast g.RemoveEdge e
            member g.RemoveEdgesFrom es = upcast g.RemoveEdgesFrom es
            member g.MergeWith graph = upcast g.MergeWith graph

    [<AbstractClass>]
    type MutableMultiGraph<'Node, 'NodeData, 'EdgeData>() =
        inherit MutableGraph<'Node, 'NodeData, MultiGraphEdgeData<'EdgeData>>()
        
        abstract member Item : MultiEdge<'Node> -> 'EdgeData with get
        abstract member RemoveMultiEdge : MultiEdge<'Node> -> unit
        abstract member RemoveMultiEdgesFrom : MultiEdge<'Node> seq -> unit
        
        interface IMultiGraph<'Node, 'NodeData, 'EdgeData> with
            member g.Item with get(k : MultiEdge<'Node>) = g.[k]

        interface IMutableMultiGraph<'Node, 'NodeData, 'EdgeData> with
            member g.RemoveMultiEdge e = g.RemoveMultiEdge e
            member g.RemoveMultiEdgesFrom es = g.RemoveMultiEdgesFrom es

    [<AbstractClass>]
    type ImmutableMultiGraph<'Node, 'NodeData, 'EdgeData>() =
        inherit ImmutableGraph<'Node, 'NodeData, MultiGraphEdgeData<'EdgeData>>()
        
        abstract member Item : MultiEdge<'Node> -> 'EdgeData with get
        abstract member RemoveMultiEdge : MultiEdge<'Node> -> ImmutableMultiGraph<'Node, 'NodeData, 'EdgeData>
        abstract member RemoveMultiEdgesFrom : MultiEdge<'Node> seq -> ImmutableMultiGraph<'Node, 'NodeData, 'EdgeData>
        
        interface IMultiGraph<'Node, 'NodeData, 'EdgeData> with
            member g.Item with get(k : MultiEdge<'Node>) = g.[k]

        interface IImmutableMultiGraph<'Node, 'NodeData, 'EdgeData> with
            member g.RemoveMultiEdge e = upcast g.RemoveMultiEdge e
            member g.RemoveMultiEdgesFrom es = upcast g.RemoveMultiEdgesFrom es