module Main
open Nessos.UnionArgParser

type Arguments =
  | Server of int
  | IncludeContent of bool
  | WorkingArea of bool
  | BaseUri of string
  | Path of string
  | Since of string
with interface IArgParserTemplate with member s.Usage =
  match s with
    | Server p -> "Run prov server on specified port"
    | IncludeContent b -> "Add content statements to file entities"
    | WorkingArea b -> "Show prov activity for uncommitted and staged "
    | BaseUri s -> "Base uri for generated provenence"
    | Path p -> "Path to a git repository"
    | Since r -> "Commit ref to generate PROV from"

let writeProv r showWorking includeContent since =
    use fout = new System.IO.StreamWriter (System.Console.OpenStandardOutput ())
    use g = new VDS.RDF.Graph ()

    TTL.ns.add (g,r) 

    let showWorkingArea ax = seq {
      match showWorking with
      | true -> yield Prov.Activity.fromWorkingArea r (Git.workingArea r)
      | false -> ()

      yield! ax
    }
    
    r |> Git.commits since 
      |> Seq.pairwise
      |> Seq.map (Prov.Activity.fromCommit r)
      |> showWorkingArea
      |> Seq.map (TTL.fromActivity includeContent g)
      |> Seq.last
      |> TTL.ttl fout
    ()

[<EntryPoint>]
let main argv = 
  let parser = UnionArgParser.Create<Arguments> ()
  let args = parser.Parse argv
  let repo = Git.repo (args.GetResult (<@ Path @>,defaultValue="."))

  let showWorking = args.GetResult (<@ WorkingArea @>,defaultValue=false)
  let includeContent = args.GetResult (<@ IncludeContent @>,defaultValue=false)
  let since = args.GetResult (<@ Since @>,defaultValue="HEAD")

  match args.GetResult (<@ Server @>,defaultValue=0) with
    | 0 -> writeProv repo showWorking includeContent since
    | port -> Http.serve repo port

  0// return an integer exit code

 
