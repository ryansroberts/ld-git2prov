module Git
open LibGit2Sharp
open System.IO

type Commit =
  | Commit of LibGit2Sharp.Commit

type Repository =
  | Repository of LibGit2Sharp.Repository

type WorkingArea =
  | WorkingArea of StatusEntry list * Commit

type Content =
  | Text of string
  | NoContent 

let repo p = Repository  ( new LibGit2Sharp.Repository (p) )

let commits t (r:Repository) =
  match r with
  | Repository r -> seq {
      for c in r.Commits.QueryBy (CommitFilter (SortBy=CommitSortStrategies.Topological,Since=t)) do
      yield Commit c
     }

let tree = function
  | Commit c -> c.Tree
  | Tail -> null :> LibGit2Sharp.Tree

let workingArea = function
  | Repository r ->
    let s = r.RetrieveStatus ()
    WorkingArea (Seq.concat
     [
      s.Added
      s.Modified
      s.Untracked
      s.Staged
      s.RenamedInIndex
      s.RenamedInWorkDir
     ] |> Seq.toList,Commit r.Head.Tip)

let diff (c,c') = function
  | Repository r ->
    r.Diff.Compare<TreeChanges>(oldTree=c,newTree=c')

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
  | Repository r ->
    r.Info.WorkingDirectory

let content (h:string) p = function
  | Repository r -> 
    let c = r.Lookup<LibGit2Sharp.Commit> h
    match c.[p] with
      | null -> NoContent
      | tr ->
        match tr.TargetType with
          | LibGit2Sharp.TreeEntryTargetType.Blob ->
            let b = tr.Target :?> LibGit2Sharp.Blob
            use r = new StreamReader(b.GetContentStream())
            Text (r.ReadToEnd ())
