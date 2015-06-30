namespace common

module RDF =
  open VDS.RDF
  open VDS.RDF.Writing
  open VDS.RDF.Parsing

  let (++) a b = System.IO.Path.Combine(a, b)

  type Uri =
    | Uri of qname : string * segments : string list * ref : string option
    override x.ToString() =
      match x with
      | Uri(q, p, Some r) -> sprintf "%s:%s#%s" q (List.reduce (++) p) r
      | Uri(q, p, None) -> sprintf "%s:/%s" q (List.reduce (++) p)
    static member fragment (Uri(_,_,Some f)) = f
  module ns =

    let add (g : IGraph, baseUri) =
      g.BaseUri <- UriFactory.Create baseUri
      [( "prov" ,"http://www.w3.org/ns/prov#" )
       ( "owl" ,"http://www.w3.org/2002/07/owl#" )
       ( "compilation" ,"http://ld.nice.org.uk/ns/compilation#" )
       ( "git2prov" ,"http://ld.nice.org.uk/prov/" )
       ( "ld" ,"http://ld.nice.org.uk/" )
       ( "dcterms" ,"http://purl.org/dc/terms/" )]
      |> List.iter (fun (p,ns) ->
                    g.NamespaceMap.AddNamespace (p,UriFactory.Create ns)
      )
  let literal (g : IGraph) s = g.CreateLiteralNode s :> INode
  let uri (g : IGraph) u = UriFactory.Create u |> g.CreateUriNode :> INode
  let puri (g : IGraph) (u : Uri) = g.CreateUriNode(string u) :> INode
  let suri (g : IGraph) (u : System.Uri) = g.CreateUriNode(u) :> INode
  let qn (g : IGraph) (qn : string) = g.CreateUriNode qn :> INode
  let a (g : IGraph) = qn g "rdf:type"
  let date (g : IGraph) (d : System.DateTimeOffset) =
    LiteralExtensions.ToLiteral(d, g) :> INode
  let triples (g : IGraph) = function
    | (s, px) ->
      px
      |> List.map (function
           | (p, o) -> Triple(s, p, o))
      |> g.Assert
      |> ignore
      ()

  let blank (g : IGraph) px =
    let b = g.CreateBlankNode()
    triples g (b, px) |> ignore
    b :> INode

  module Store =
    open VDS.RDF
    open VDS.RDF.Query
    open VDS.RDF.Query.Datasets
    open VDS.RDF.Storage
    open VDS.RDF.Storage.Management
    open VDS.RDF.Parsing

    let parser = SparqlQueryParser()

    type store =
      | Memory of IGraph
      member x.QueryProcessor() =
        match x with
        | Memory m -> LeviathanQueryProcessor(InMemoryDataset(m))

    let defaultUri = null :> System.Uri
    let loadGraph (g : Graph) = store.Memory g

    let loadFile (s : string) =
      let g = new Graph()
      match s.StartsWith("http") with
      | true -> g.LoadFromUri(System.Uri s)
      | false -> g.LoadFromFile s
      Memory g

    let loadTtl (s : System.IO.Stream) =
      let g = new Graph()
      let p = new TurtleParser()
      use sr = new System.IO.StreamReader(s)
      p.Load(g, sr)
      Memory g

    let query (store : store) (q : string) =
      (store.QueryProcessor()).ProcessQuery(parser.ParseFromString(q))
    let construct (store : store) q = query store q :?> IGraph
    let resultset (store : store) q = query store q :?> SparqlResultSet

    let dump g =
      let s = System.Text.StringBuilder()
      let w = new VDS.RDF.Writing.CompressingTurtleWriter()
      use sw = new System.IO.StringWriter(s)
      w.Save(g, sw)
      s.ToString()

    let diff g g' =
      match g, g' with
      | Memory g, Memory g' -> g.Difference g'
