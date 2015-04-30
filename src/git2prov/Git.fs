module Git

open LibGit2Sharp
open System.IO

let (++) a b = System.IO.Path.Combine(a, b)

type Commit =
    | Commit of LibGit2Sharp.Commit

type Repository =
    | Repository of LibGit2Sharp.Repository

type WorkingArea =
    | WorkingArea of StatusEntry list * Commit

type Content =
    | Revision of System.Uri
    | Working of System.Uri

type Path =
    | Path of string
    override x.ToString() =
        match x with
        | Path p -> p

let repo p = Repository(new LibGit2Sharp.Repository(p))

let commits (since : string) r =
    match r with
      | Repository r ->
        let c = match since with
          | "all" ->
            (null :> LibGit2Sharp.Commit)
          | since ->
            let c = r.Lookup<LibGit2Sharp.Commit>(since)
            if(c = null) then (failwithf "Cannot locate commit with hash %s" since)
            c
        seq {
            for c in r.Commits.QueryBy
                         (CommitFilter (Until = c,
                                        SortBy = (CommitSortStrategies.Topological ||| CommitSortStrategies.Time))) ->
              Commit c
            yield Commit (r.Head.Tip)
        }

let tree = function
    | Commit c -> c.Tree
let workingArea =
    function
    | Repository r ->
        let s = r.RetrieveStatus()
        WorkingArea
            (Seq.concat
                 [ s.Added; s.Modified; s.Untracked; s.Staged; s.RenamedInIndex;
                   s.RenamedInWorkDir ] |> Seq.toList, Commit r.Head.Tip)
let diff (c, c') = function
  | Repository r ->
    r.Diff.Compare<TreeChanges>(oldTree = c, newTree = c')
let diffs cx = function
    | Repository r ->
        cx
        |> Seq.map tree
        |> Seq.pairwise
        |> Seq.map diff
let branchName = function
    | Repository r ->
        match r.Head.Name with
        | null -> r.Head.Tip.Sha
        | name -> name
let workingDirectory = function
    | Repository r -> Path r.Info.WorkingDirectory
let directoryName = function
    | Path p -> p.Split(Path.DirectorySeparatorChar) |> Seq.last
let content (h : string) p = function
    | Repository r -> Revision(System.Uri (sprintf "http://raw.git.nice.org.uk/%s:%s" h p))
let unstagedContent (f : LibGit2Sharp.StatusEntry) = function
    | Repository r -> Working(System.Uri (sprintf "file://%s%s" (r.Info.WorkingDirectory) (f.FilePath)))
