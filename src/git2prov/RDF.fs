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
            | Uri(q, p, Some r) -> 
                sprintf "%s:%s%s" q (p |> String.concat "/") r
            | Uri(q, p, None) -> sprintf "%s:/%s" q (p |> String.concat "/")
    
    module ns = 
        let prov = "http://www.w3.org/ns/prov#"
        let owl = "http://www.w3.org/2002/07/owl#"
        let cnt = "http://www.w3.org/2011/content#"
        let compilation = "http://nice.org.uk/ns/compilation#"
        let git2prov = "http://nice.org.uk/ns/prov#"
        
        let add (g : IGraph, baseUri) = 
            g.BaseUri <- UriFactory.Create baseUri
            g.NamespaceMap.AddNamespace("prov", UriFactory.Create prov)
            g.NamespaceMap.AddNamespace("owl", UriFactory.Create owl)
            g.NamespaceMap.AddNamespace("git2prov", UriFactory.Create git2prov)
            g.NamespaceMap.AddNamespace("base", UriFactory.Create baseUri)
            g.NamespaceMap.AddNamespace
                ("compilation", UriFactory.Create compilation)
            g.NamespaceMap.AddNamespace("cnt", UriFactory.Create cnt)
    
    let literal (g : IGraph) s = g.CreateLiteralNode s :> INode
    let uri (g : IGraph) u = UriFactory.Create u |> g.CreateUriNode :> INode
    let puri (g : IGraph) (u : Uri) = g.CreateUriNode(string u) :> INode
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
