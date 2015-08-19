module Main

open Nessos.UnionArgParser
open common
open common.RDF

open OpenFileSystem.IO
type FileSystem =
  | FileSystem of IFileSystem
  with static member unix () = FileSystem (new FileSystems.Local.Unix.UnixFileSystem ())


type Arguments =
  | ShowTree
  | IncludeWorkingArea
  | ShowHistory
  | ExistingProv of string
  | Path of string
  | Since of string
  | Output of string
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | ShowTree  -> "Tree statements for reachable resources at the specified version"
      | IncludeWorkingArea _ -> "Include prov activity for uncommitted and staged "
      | ExistingProv _ -> "Location of stored prov"
      | ShowHistory -> "Include git history"
      | Path p -> "Path to a git repository"
      | Since r -> "Commit ref to generate PROV from"
      | Output o -> "Where to write graphs"

let (++) a b = System.IO.Path.Combine (a,b)

let provDirectory (fs:IFileSystem) (p:string) (a:Prov.Activity) =
  (fs.GetDirectory p).GetOrCreateDirectory (Uri.fragment a.Id)

let historyFile (FileSystem fs) p a = (provDirectory fs p a).GetFile "prov.ttl"
let treeFile (FileSystem fs) p a = (provDirectory fs p a).GetFile "tree.prov.ttl"

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

let existingProv (FileSystem fs) path =
  fs.GetDirectory(path)
    .Directories("*",SearchScope.CurrentOnly)
  |> Seq.map (fun p -> p.Name)
  |> Set.ofSeq

let writeProv fs existing path repo showTree showHistory includeWorking since =
  let existingProv = existingProv fs existing
  let working = if includeWorking then seq {yield working repo} else Seq.empty
  let history = if showHistory then
                  history repo since
                  |> Seq.filter (fun (a,_) ->
                                 not(Set.contains (Option.get a.Hash ) existingProv))
                else Seq.empty

  for (a,xs) in Seq.concat [working;history] do
    use hout = (historyFile fs path a).OpenWrite()
    use g = new VDS.RDF.Graph()
    RDF.ns.add (g, "http://ld.nice.org.uk/prov")

    Translate.provHistory g a
    Translate.provCompilation g (Prov.Activity.compilation a)
    Translate.ttl hout g

    printfn "%s" (Uri.fragment a.Id)

    if showTree then
        use tout = (treeFile fs path a).OpenWrite ()
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
  let showTree = args.Contains(<@ ShowTree @>)
  let since = args.GetResult(<@ Since @>, defaultValue = "HEAD")
  let output = args.GetResult(<@ Output @>, defaultValue = ".")
  let existing = args.GetResult(<@ ExistingProv @>, defaultValue = output)

  writeProv (FileSystem.unix ()) existing output repo showTree showHistory includeWorking since
  0
