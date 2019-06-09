namespace GraphFS

open GraphFS.Core
open GraphFS.Core.VertexSet
open GraphFS.Core.EdgeSet
open GraphFS.Core.MultiEdgeSet

module Graph =
    type Graph<'V, 'VS, 'ES when 'VS :> IVertexSet<'V> and 'ES :> IEdgeSet<'V>>(V: 'VS, E: 'ES) =
        member __.V = V
        member __.E = E
        static member applyV f (g : Graph<'V, 'VS, 'ES>) = f g.V
        static member applyE f (g : Graph<'V, 'VS, 'ES>) = f g.E
        static member mapV f (g : Graph<'V, 'VS, 'ES>) = Graph(f g.V, g.E)
        static member mapE f (g : Graph<'V, 'VS, 'ES>) = Graph(g.V, f g.E)
        static member mapVE f1 f2 (g : Graph<'V, 'VS, 'ES>) = Graph(f1 g.V, f2 g.E)

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
        
        let fromSets V E = Graph (V, E)
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
