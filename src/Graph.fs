namespace FSharp.Data

module private Utils =

  let flip f a b = f b a

  let mapKeys f =
    Map.toList
      >> List.map (fun (key, value) -> f key, value)
      >> Map.ofList

  type OptionBuilder() =
    member this.Return(x) = Some x
    
    member this.ReturnFrom(x) = x

    member this.Bind(option, f) = Option.bind f option

    member this.Zero() = None

  let option = OptionBuilder()

module Graph =
  open Utils

  type private GraphInternal<'vertex, 'edge> when 'vertex : comparison = 
    { Edges: Map<int, Map<int, 'edge>>
      EdgeData: Map<int*int, 'edge>
      Vertices: Map<'vertex, int>
      VerticesById: Map<int, 'vertex>
      UnusedId: int }
  
  type Graph<'vertex, 'edge> when 'vertex : comparison =
    private Graph of GraphInternal<'vertex, 'edge>

  type Edge<'vertex, 'edge> =
    { From: 'vertex
      To: 'vertex
      Data: 'edge }

  let emptyGraph: Graph<'vertex, 'edege> = 
    Graph 
      { Edges = Map.empty
        EdgeData = Map.empty 
        Vertices = Map.empty
        VerticesById = Map.empty
        UnusedId = 0 }        

  let isEmpty (Graph g: Graph<'vertex, 'edege>) = Map.isEmpty g.Vertices

  let hasVertex (vertex: 'vertex) (Graph g: Graph<'vertex, 'edege>) = Map.containsKey vertex g.Vertices

  let hasEdge (fromVertex: 'vertex) (toVertex: 'vertex) (Graph g: Graph<'vertex, 'edege>) = 
    option {
      let! edgesFrom = 
        Map.tryFind fromVertex g.Vertices
          |> Option.bind (flip Map.tryFind g.Edges)

      let! toId = Map.tryFind toVertex g.Vertices

      return Map.containsKey toId edgesFrom
    }
    |> Option.defaultValue false

  let addVertex (vertex: 'vertex) (Graph g: Graph<'vertex, 'edge>) =
    Graph
      { g with
          Vertices = g.Vertices |> Map.add vertex g.UnusedId
          VerticesById = g.VerticesById |> Map.add g.UnusedId vertex
          UnusedId = g.UnusedId + 1 }

  let addEdge (fromVertex: 'vertex) (toVertex: 'vertex) (edge: 'edge) graph =
    let addEdge' from _to edge' (Graph g' as graph') =
      option {
        let! fromId = Map.tryFind from g'.Vertices
        let! toId = Map.tryFind _to g'.Vertices
        let! e = Map.tryFind toId g'.Edges

        return Graph 
          { g' with 
              Edges = g'.Edges |> Map.add fromId (Map.add toId edge' e) 
              EdgeData = g'.EdgeData |> Map.add (fromId, toId) edge' }
      }
      |> Option.defaultValue graph'
    
    graph
      |> addVertex fromVertex
      |> addVertex toVertex
      |> addEdge' fromVertex toVertex edge
 
  let removeVertex (vertex: 'vertex) (Graph g: Graph<'vertex, 'edge> as graph) =
    option {
      let! vId = Map.tryFind vertex g.Vertices
      let newEdges =
        g.Edges
          |> Map.filter (fun fromId _ -> fromId <> vId)
          |> Map.map (fun _ toIds -> Map.remove vId toIds)

      return Graph
        { g with
            Vertices = Map.remove vertex g.Vertices
            VerticesById = Map.remove vId g.VerticesById
            Edges = newEdges }
    }
    |> Option.defaultValue graph

  let removeEdge (fromVertex: 'vertex) (toVertex: 'vertex) (Graph g: Graph<'vertex, 'edge> as graph) =
    option {
      let! fromId = Map.tryFind fromVertex g.Vertices
      let! toId = Map.tryFind toVertex g.Vertices
      let! innerEdges = Map.tryFind fromId g.Edges

      return Graph 
        { g with 
            Edges = g.Edges |> Map.add fromId (Map.remove toId innerEdges) 
            EdgeData = g.EdgeData |> Map.remove (fromId, toId) }
    }
    |> Option.defaultValue graph

  let areAdjacent (v1: 'vertex) (v2: 'vertex) (graph: Graph<'vertex, 'edge>) =
    hasEdge v1 v2 graph || hasEdge v2 v1 graph

  let getEdge (fromVertex: 'vertex) (toVertex: 'vertex) (Graph g: Graph<'vertex, 'edge>) =
    option {
      let! fromId = Map.tryFind fromVertex g.Vertices
      let! toId = Map.tryFind toVertex g.Vertices

      return! Map.tryFind (fromId, toId) g.EdgeData
    }

  let updateEdge (fromVertex: 'vertex) (toVertex: 'vertex) (f: 'edge -> 'edge) (Graph g as graph) = 
    option {
      let! fromId = Map.tryFind fromVertex g.Vertices
      let! toId = Map.tryFind toVertex g.Vertices
      let! e = Map.tryFind (fromId, toId) g.EdgeData

      return Graph 
        { g with EdgeData = g.EdgeData |> Map.add (fromId, toId) (f e) }
    }
    |> Option.defaultValue graph

  let fromVerticesAndEdges (vertices: 'vertex list) (edges: Edge<'vertex, 'edge> list) =
    List.fold (fun state {From=from; To=_to; Data=data} -> state |> addEdge from _to data) emptyGraph edges
      |> fun g -> List.fold (flip addVertex) g vertices

  let size (Graph g: Graph<'vertex, 'edge>) = Map.count g.Vertices

  let verticies (Graph g: Graph<'vertex, 'edge>) = Map.toList g.Vertices |> List.map fst

  let edges (Graph g) =
    g.Edges
      |> Map.toList
      |> List.collect (fun (from, _tos) ->
          _tos
            |> Map.toList
            |> List.choose (fun (_to', _) -> option {
                let! fromId = Map.tryFind from g.VerticesById
                let! toId = Map.tryFind _to' g.VerticesById
                let! data = Map.tryFind (fromId, toId) g.EdgeData

                return { From=fromId; To=toId; Data=data }
              }
            )
        )

  let mapVertices (f: 'v1 -> 'v2) (Graph g : Graph<'v1, 'edge>) : Graph<'v2, 'edge> when 'v2 : comparison =
    Graph
      { EdgeData = g.EdgeData
        Edges = g.Edges
        VerticesById = Map.map (fun _ v -> f v) g.VerticesById
        Vertices = mapKeys f g.Vertices 
        UnusedId = g.UnusedId }

  let mapEdges (f: 'e1 -> 'e2) (Graph g : Graph<'vertex, 'e1>) : Graph<'vertex, 'e2> when 'e2 : comparison =
    let newEdges = 
      g.Edges |> Map.map (fun _ edges' -> edges' |> Map.map (fun _ e -> f e))

    Graph 
      { EdgeData = g.EdgeData |> Map.map (fun _ v -> f v)
        Edges = newEdges
        Vertices = g.Vertices
        VerticesById = g.VerticesById
        UnusedId = g.UnusedId }

  let fold f (acc: 'state) (Graph g: Graph<'vertex, 'edege>) =
    Map.fold (fun state _ value -> f state value) acc g.VerticesById

  let dijkstra _ = raise <| System.NotImplementedException ()

  let dfs _ = raise <| System.NotImplementedException ()