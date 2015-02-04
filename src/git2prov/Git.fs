module Git
  open LibGit2Sharp

  type Commit =
    | Commit of LibGit2Sharp.Commit
    
  type Repository =
    | Repository of LibGit2Sharp.Repository

  type WorkingArea =
    | WorkingArea of StatusEntry list * Commit

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
 
