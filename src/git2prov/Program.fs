namespace Git2Prov

module Git =
  open LibGit2Sharp

  type Commit =
    | Commit of LibGit2Sharp.Commit
    
  type Repository =
  | Repository of LibGit2Sharp.Repository

  let repo p = Repository  ( new LibGit2Sharp.Repository (p) )

  let commits t (r:Repository) =
    match r with
    | Repository r -> seq {
        for c in r.Commits.QueryBy (LibGit2Sharp.CommitFilter (SortBy=LibGit2Sharp.CommitSortStrategies.Topological,Since=t)) do
        yield Commit c
       }
    
  let tree = function
    | Commit c -> c.Tree
    | Tail -> null :> LibGit2Sharp.Tree

  let diff (c,c') = function
    | Repository r ->
      r.Diff.Compare<TreeChanges>(oldTree=c,newTree=c')

  let diffs cx = function
    | Repository r ->
      cx
      |> Seq.map tree
      |> Seq.pairwise
      |> Seq.map diff
  
module Prov =
  open Git
  open System

  let short sha = function
    | Repository r ->
      r.ObjectDatabase.ShortenObjectId sha

 
  type Uri =
    | Uri of string 
    with static member commit r c = Uri (sprintf "commit/%s" (short c r))
         static member identity (c:LibGit2Sharp.Commit) = Uri (sprintf "user/%s" c.Author.Email)
         static member versionedcontent (c,p) = Uri (sprintf "commit/%s/%s" c p)
         static member individual (r,f:LibGit2Sharp.TreeEntryChanges) = Uri (f.Path)
         override x.ToString () = match x with | Uri s -> sprintf "base:%s" s 
         
  type FileVersion = {
    Id : Uri
    Content : string
    PreviousVersion : string option
    SpecialisationOf : Uri
    Commit : Uri
    AttributedTo : Uri
    }
  with static member from (c:LibGit2Sharp.Commit) (c':LibGit2Sharp.Commit) r = seq {
    let d = diff (c.Tree,c'.Tree) r
    for f in d.Modified do yield {
      Id = Uri.versionedcontent (f.Oid.Sha,f.Path)  
      Content = ""
      PreviousVersion = Some f.OldOid.Sha 
      SpecialisationOf = Uri.individual (r,f) 
      Commit = Uri.commit r c
      AttributedTo = Uri.identity c 
      }
    }
  
  type Activity = {
    Id : Uri
    Time : DateTimeOffset
    Label : string
    User : Uri
    Used : FileVersion seq
    InformedBy : Uri seq
    }
  with static member fromCommit r = function
    | Commit c,Commit c' -> {
        Id = Uri.commit r c
        Time = c.Author.When
        Label = c.Message
        User = Uri.identity c
        Used = FileVersion.from c c' r
        InformedBy = c.Parents |> Seq.map (Uri.commit r)
        }

module TTL =
  open Prov
  open Git
  open VDS.RDF
  open VDS.RDF.Writing

  module ns = 
    
    let prov = "http://www.w3.org/ns/prov#"
    let owl = "http://www.w3.org/2002/07/owl#"
    let bas = "http://nice.org.uk/ns/prov#"

    let add (g:IGraph,r) =
      match r with
        | Repository r ->
          let name = System.IO.Path.GetDirectoryName r.Info.WorkingDirectory 
          let tree = r.Head.TrackedBranch.Name
          let bas = sprintf "%s/tree/%s/" name tree  
          g.BaseUri <- UriFactory.Create bas
          g.NamespaceMap.AddNamespace ("prov",UriFactory.Create prov)
          g.NamespaceMap.AddNamespace ("owl",UriFactory.Create owl)
          g.NamespaceMap.AddNamespace ("base",UriFactory.Create bas)
      
  let fromActivity  (g:IGraph) (act:Activity) =
   
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

    for u in act.Used do
      triples (puri act.Id,[qn "prov:uses",puri u.Id])
      triples (puri u.Id,[
        (a, qn "prov:Entity")
        (qn "prov:wasGeneratedBy",puri u.Commit)
        (qn "prov:wasAttributedTo",puri u.AttributedTo)
        (qn "prov:specializationOf",puri u.SpecialisationOf)
        ])
    g

  let ttl (tw:System.IO.TextWriter) g =
    let writer = CompressingTurtleWriter (CompressionLevel=3,PrettyPrintMode=true)
    writer.Save (g,tw) 
      
module Main = 
  open Nessos.UnionArgParser
  open Git
  
  type Arguments =
    | BaseUri of string
    | Path of string
    | Since of string
  with interface IArgParserTemplate with
    member s.Usage =
      match s with
        | BaseUri s -> "Base uri for generated provenence"
        | Path p -> "Path to a git repository"
        | Since r -> "Commit ref to generate PROV from"
        
  [<EntryPoint>]
  let main argv = 
    let parser = UnionArgParser.Create<Arguments>()
    let args = parser.Parse argv
    let repo = repo (args.GetResult (<@ Path @>,defaultValue="."))
    use fout = new System.IO.StreamWriter (System.Console.OpenStandardOutput())
    use g = new VDS.RDF.Graph ()
    TTL.ns.add (g,repo) 
    repo
    |> commits (args.GetResult (<@ Since @>,defaultValue="HEAD"))
    |> Seq.pairwise
    |> Seq.map (Prov.Activity.fromCommit repo)
    |> Seq.map (TTL.fromActivity g)
    |> Seq.last
    |> TTL.ttl fout
    
    0// return an integer exit code

