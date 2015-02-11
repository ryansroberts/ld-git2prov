module TTL
  open Prov
  open Git
  open VDS.RDF
  open VDS.RDF.Writing

  module ns = 
    
    let prov = "http://www.w3.org/ns/prov#"
    let owl = "http://www.w3.org/2002/07/owl#"
    let bas = "http://nice.org.uk/ns/prov#"
    let cnt = "http://www.w3.org/2011/content#"

    let add (g:IGraph,r) =
        let name = System.IO.Path.GetDirectoryName ( Git.workingDirectory r ) 
        let tree = Git.branchName r
        let bas = sprintf "%s/tree/%s/" name tree  
        g.BaseUri <- UriFactory.Create bas
        g.NamespaceMap.AddNamespace ("prov",UriFactory.Create prov)
        g.NamespaceMap.AddNamespace ("owl",UriFactory.Create owl)
        g.NamespaceMap.AddNamespace ("base",UriFactory.Create bas)
        g.NamespaceMap.AddNamespace ("cnt",UriFactory.Create cnt)
      
  let fromActivity includeContent (g:IGraph) (act:Activity) =
   
    let literal s =
      g.CreateLiteralNode s :> INode

    let uri u =
      UriFactory.Create u
      |> g.CreateUriNode :> INode

    let puri (u:Prov.Uri) =
      g.CreateUriNode (string u) :> INode

    let qn (qn:string) = g.CreateUriNode qn :> INode

    let a = qn "rdf:type"

    let date (d:System.DateTimeOffset) = LiteralExtensions.ToLiteral (d,g) :> INode 

    let triples = function
      | (s,px) ->
        px
        |> List.map (function | (p,o) -> Triple (s,p,o))
        |> g.Assert
        |> ignore
        ()
        
    let blank px = 
      let b = g.CreateBlankNode ()
      
      triples (b,px)
      |> ignore
      b :> INode

    triples (puri act.Id,[
        (a,qn "prov:Activity")
        (qn "prov:startedAtTime",date act.Time)
        (qn "prov:endedAtTime",date act.Time)
        (qn "prov:wasAssociatedWith",puri act.User)
        (qn "prov:qualifiedAssociation", blank [
           (a,qn "prov:Association")
           (qn "prov:agent",puri act.User)
           (qn "prov:hadRole",literal "author, committer")
           ])
        ])
    
    for i in act.InformedBy do
      triples (puri act.Id,[qn "prov:informedBy",puri i])

    for u in act.Used do
      triples (puri act.Id,[qn "prov:uses",puri u.Id])
      triples (puri u.Id,[
        (a, qn "prov:Entity")
        (qn "prov:wasGeneratedBy",puri u.Commit)
        (qn "prov:wasAttributedTo",puri u.AttributedTo)
        (qn "prov:specializationOf",puri u.SpecialisationOf)
                                  
        ])
      match u.Content,includeContent with
        | Content.Text t, true ->
          triples (puri u.Id,[(qn "cnt:chars",literal t)])
        | _,_ -> ()
    g

  let ttl (tw:System.IO.TextWriter) g =
    let writer = CompressingTurtleWriter (CompressionLevel=3,PrettyPrintMode=true)
    writer.Save (g,tw)

