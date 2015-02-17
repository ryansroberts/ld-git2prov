module Main

open Nessos.UnionArgParser

type Arguments = 
  | Server of int
  | IncludeWorkingArea
  | ShowHistory
  | ShowCompilation
  | ShowContent
  | BaseUri of string
  | Path of string
  | Since of string
  interface IArgParserTemplate with
    member s.Usage = 
      match s with
      | Server p -> "Run prov server on specified port"
      | IncludeWorkingArea _ -> 
        "Include prov activity for uncommitted and staged "
      | ShowHistory _ -> "Show provenence for included git history"
      | ShowCompilation _ -> 
        "Generate a compilation activity for included git history"
      | ShowContent _ -> "Add content statements to file entities"
      | BaseUri s -> "Base uri for generated provenence"
      | Path p -> "Path to a git repository"
      | Since r -> "Commit ref to generate PROV from"

let gatherProv r includeWorking since = 
  let includeWorkingArea ax = 
    seq { 
      match includeWorking with
      | true -> yield Prov.Activity.fromWorkingArea r (Git.workingArea r)
      | false -> ()
      yield! ax
    }
  r
  |> Git.commits since
  |> Seq.pairwise
  |> Seq.map (Prov.Activity.fromCommit r)
  |> includeWorkingArea

let writeProv repo showContent showHistory showCompilation prov = 
  use fout = new System.IO.StreamWriter(System.Console.OpenStandardOutput())
  use g = new VDS.RDF.Graph()
  RDF.ns.add (g, repo)
  let history() = 
    prov
    |> Seq.map (RDF.provHistory showContent g)
    |> Seq.last
    |> ignore
  
  let compilation() = 
    prov
    |> Prov.Activity.concat
    |> RDF.provCompilation g
    |> ignore
  
  match showHistory, showCompilation with
  | true, _ | _, true -> history()
  | _, _ -> ()
  match showCompilation with
  | true -> compilation()
  | false -> ()
  g |> RDF.ttl fout
  ()

[<EntryPoint>]
let main argv = 
  let parser = UnionArgParser.Create<Arguments>()
  let args = parser.Parse argv
  let repo = Git.repo (args.GetResult(<@ Path @>, defaultValue = "."))
  let includeWorking = args.Contains(<@ IncludeWorkingArea @>)
  let showContent = args.Contains(<@ ShowContent @>)
  let showHistory = args.Contains(<@ ShowHistory @>)
  let showCompilation = args.Contains(<@ ShowCompilation @>)
  let since = args.GetResult(<@ Since @>, defaultValue = "HEAD")
  match args.GetResult(<@ Server @>, defaultValue = 0) with
  | 0 -> 
    gatherProv repo includeWorking since 
    |> writeProv repo showContent showHistory showCompilation
  | port -> Http.serve repo port
  0 // return an integer exit code
