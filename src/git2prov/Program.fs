module Main

open Nessos.UnionArgParser
open common

type Arguments =
    | Server of int
    | IncludeWorkingArea
    | ShowHistory
    | ShowCompilation
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
    Git.commits since r
    |> Seq.pairwise
    |> Seq.map (Prov.Activity.fromCommit r)
    |> includeWorkingArea

let branchNs (g : VDS.RDF.IGraph, r) =
    let name = Git.directoryName (Git.workingDirectory r)
    let tree = Git.branchName r
    let git2prov = sprintf "http://nice.org.uk/git2prov/%s/tree/%s" name tree
    g.NamespaceMap.AddNamespace("tree", VDS.RDF.UriFactory.Create git2prov)

let writeProv repo showHistory showCompilation prov =
    let sio =System.Console.OpenStandardOutput()
    use fout = new System.IO.StreamWriter(sio)
    use g = new VDS.RDF.Graph()
    RDF.ns.add (g, RDF.ns.git2prov)
    branchNs (g, repo)
    let history() =
        prov
        |> Seq.map (Translate.provHistory g)
        |> Seq.iter (fun _ -> ())

    let compilation() =
        prov
        |> Prov.Activity.concat
        |> Translate.provCompilation g
        |> ignore

    match showHistory, showCompilation with
    | true, _ | _, true -> history()
    | _, _ -> ()
    match showCompilation with
    | true -> compilation()
    | false -> ()
    g |> Translate.ttl fout
    ()

[<EntryPoint>]
let main argv =
    let toLower (s:string) = s.ToLower()
    let containsParam param = Seq.map toLower >> Seq.exists ((=) (toLower param))
    let paramIsHelp param = containsParam param ["help"; "?"; "/?"; "-h"; "--help"; "/h"; "/help"]

    let parser = UnionArgParser.Create<Arguments>()

    if (( argv.Length = 2 && paramIsHelp argv.[1] ) || argv.Length = 1) then
      printfn """Usage: git2prov [options]
                 %s""" ( parser.Usage ()  )
      exit 1

    let args = parser.Parse argv
    let repo = Git.repo (args.GetResult(<@ Path @>, defaultValue = "."))
    let includeWorking = args.Contains(<@ IncludeWorkingArea @>)
    let showHistory = args.Contains(<@ ShowHistory @>)
    let showCompilation = args.Contains(<@ ShowCompilation @>)
    let since = args.GetResult(<@ Since @>, defaultValue = "HEAD")
    match args.GetResult(<@ Server @>, defaultValue = 0) with
    | 0 ->
        gatherProv repo includeWorking since
        |> writeProv repo showHistory showCompilation
    | port -> Http.serve repo port
    0
