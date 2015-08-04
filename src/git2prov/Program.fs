module Main

open Nessos.UnionArgParser
open common
open common.RDF

type Arguments =
  | Tree of string
  | IncludeWorkingArea
  | ShowHistory
  | BaseUri of string
  | Path of string
  | Since of string
  | Output of string
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Tree _ -> "treeAtCommit statements for reachable resources at the specified version"
      | IncludeWorkingArea _ -> "Include prov activity for uncommitted and staged "
      | ShowHistory -> "Include git history"
      | BaseUri s -> "Base uri for generated provenence"
      | Path p -> "Path to a git repository"
      | Since r -> "Commit ref to generate PROV from"
      | Output o -> "Where to write graphs"

let (++) a b = System.IO.Path.Combine (a,b)

let historyName b (a:Prov.Activity) = b ++ (sprintf "%s.prov.ttl" (Uri.fragment a.Id))
let treeName b (a:Prov.Activity) = b ++ (sprintf "%s.tree.prov.ttl" (Uri.fragment a.Id))

let working r =
  let w = Git.workingArea r
  (Prov.Activity.fromWorkingArea r w,Prov.TreeFile.fromWorkingArea r w)

let history r since =
   Git.commits since r
   |> Seq.map (fun c -> (Prov.Activity.fromCommit r c,Prov.TreeFile.from r c))

let treeAtCommit (a:Prov.Activity) (xs:Prov.TreeFile seq) =
  let g = new VDS.RDF.Graph()
  g.BaseUri <- System.Uri("http://ld.nice.org.uk/prov/tree#" + (Uri.fragment (a.Id)))
  RDF.ns.add (g, "http://ld.nice.org.uk/prov")

  for x in xs do
   Translate.treeAtCommit g x |> ignore
  g

let existingProv path =
  System.IO.Directory.EnumerateFiles (path,"*.prov.ttl")
  |> Seq.map System.IO.Path.GetFileName
  |> Seq.map (fun s -> s.Split([|'.'|]) |> Seq.head)
  |> Set.ofSeq

let writeProv path repo showHistory includeWorking since =
  let existingProv = existingProv path
  let working = if includeWorking then seq {yield working repo} else Seq.empty
  let history = if showHistory then
                  history repo since
                  |> Seq.filter (fun (a,_) ->
                                 not(Set.contains (Option.get a.Hash ) existingProv))
                else Seq.empty

  for (a,xs) in Seq.concat [working;history] do
    use hout = System.IO.File.OpenWrite (historyName path a)
    use tout = System.IO.File.OpenWrite (treeName path a)
    use g = new VDS.RDF.Graph()
    RDF.ns.add (g, "http://ld.nice.org.uk/prov")

    Translate.provHistory g a
    Translate.provCompilation g (Prov.Activity.compilation a)
    Translate.ttl hout g

    printfn "%s" (Uri.fragment a.Id)

    treeAtCommit a xs
    |> Translate.ttl tout

  ()

[<EntryPoint>]
let main argv =
  let toLower (s : string) = s.ToLower()
  let containsParam param = Seq.map toLower >> Seq.exists ((=) (toLower param))
  let paramIsHelp param =
    containsParam param [ "help"; "?"; "/?"; "-h"; "--help"; "/h"; "/help" ]
  let parser = UnionArgParser.Create<Arguments>()
  if ((argv.Length = 2 && paramIsHelp argv.[1]) || argv.Length = 1) then
    printfn """Usage: git2prov [options]
                 %s""" (parser.Usage())
    exit 0


  let args = parser.Parse argv
  let repo = Git.repo (args.GetResult(<@ Path @>, defaultValue = "."))

  let includeWorking = args.Contains(<@ IncludeWorkingArea @>)
  let showHistory = args.Contains(<@ ShowHistory @>)
  let since = args.GetResult(<@ Since @>, defaultValue = "HEAD")
  let output = args.GetResult(<@ Output @>, defaultValue = ".")

  writeProv output repo showHistory includeWorking since
  0
