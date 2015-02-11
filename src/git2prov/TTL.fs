module RDF 
open Prov
open Git
open VDS.RDF
open VDS.RDF.Writing

module ns = 
  let prov = "http://www.w3.org/ns/prov#"
  let owl = "http://www.w3.org/2002/07/owl#"
  let bas = "http://nice.org.uk/ns/prov#"
  let cnt = "http://www.w3.org/2011/content#"
  let compilation = "http://nice.org.uk/ns/compilation#"


  let add (g:IGraph,r) =
      let name = Git.directoryName ( Git.workingDirectory r) 
      let tree = Git.branchName r
      g.BaseUri <- UriFactory.Create bas
      let git2prov = sprintf "http://nice.org.uk/git2prov/%s/tree/%s" name tree  
      g.NamespaceMap.AddNamespace ("prov",UriFactory.Create prov)
      g.NamespaceMap.AddNamespace ("owl",UriFactory.Create owl)
      g.NamespaceMap.AddNamespace ("git2prov",UriFactory.Create git2prov)
      g.NamespaceMap.AddNamespace ("base",UriFactory.Create bas)
      g.NamespaceMap.AddNamespace ("compilation",UriFactory.Create bas)
      g.NamespaceMap.AddNamespace ("cnt",UriFactory.Create cnt)

let literal (g:IGraph) s =
    g.CreateLiteralNode s :> INode

let uri (g:IGraph) u =
    UriFactory.Create u
    |> g.CreateUriNode :> INode

let puri(g:IGraph) (u:Prov.Uri) =
    g.CreateUriNode (string u) :> INode

let qn (g:IGraph) (qn:string) = g.CreateUriNode qn :> INode

let a (g:IGraph) = qn g "rdf:type"

let date (g:IGraph)(d:System.DateTimeOffset) = LiteralExtensions.ToLiteral (d,g) :> INode 

let triples (g:IGraph) = function
    | (s,px) ->
      px
      |> List.map (function | (p,o) -> Triple (s,p,o))
      |> g.Assert
      |> ignore
      ()

let blank (g:IGraph) px = 
    let b = g.CreateBlankNode ()

    triples g (b,px)
    |> ignore
    b :> INode


let provHistory includeContent (g:IGraph) (act:Activity) =

    let triples = triples g
    let puri = puri g
    let date = date g
    let a = a g
    let qn = qn g
    let blank = blank g
    let literal = literal g

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

let provCompilation (g:IGraph) (act:Activity) =

    let triples = triples g
    let puri = puri g
    let date = date g
    let a = a g
    let qn = qn g
    let blank = blank g
    let literal = literal g

    triples (puri act.Id,[
       (a,qn "compilation:Compilation")
       (qn "prov:startedAtTime",date act.Time)
       (qn "prov:wasAssociatedWith",puri act.User)
       (qn "prov:qualifiedAssociation", blank [
          (a,qn "prov:Association")
          (qn "prov:agent",puri act.User)
          (qn "prov:hadRole",literal "initiator")
          ])
       ])

    for u in act.Used do
      triples (puri act.Id,[qn "prov:uses",puri u.Id])

    g


let ttl (tw:System.IO.TextWriter) g =
    let writer = CompressingTurtleWriter (CompressionLevel=3,PrettyPrintMode=true)
    writer.Save (g,tw)

