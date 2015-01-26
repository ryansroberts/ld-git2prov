namespace Git2Prov

module Git =
  open LibGit2Sharp

  type Commit =
    | Commit of LibGit2Sharp.Commit
    | Tail
    
  type Repository =
  | Repository of LibGit2Sharp.Repository

  let repo p f =
    use r = new LibGit2Sharp.Repository (p)
    f <| Repository r

  let commits t (r:Repository) =
    match r with
    | Repository r -> seq {
        yield Tail
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
    | Uri of string * string
    with static member commit c r = Uri ("commit:",short c r)
         static member identity (c:LibGit2Sharp.Commit) = Uri ("identity:",c.Author.Email)
         static member versionedcontent c r = Uri ("versionedcontent:",short c r)
         
  type FileVersion = {
    Id : Uri
    Content : string
    PreviousVersion : string option
    SpecialisationOf : Uri
    Commit : Uri
    AttributedTo : Uri
    }
  with static member fromDiff (c:LibGit2Sharp.Commit) (c':LibGit2Sharp.Commit) (d:LibGit2Sharp.TreeChanges) r = seq {
      for f in d.Modified do yield {
        Id = Uri.versionedcontent (LibGit2Sharp.GitObject f.Oid) r 
        Content = ""
        PreviousVersion = Some 
        SpecialisationOf = Uri ""
        Commit = Uri.commit c r
        AttributedTo = Uri ""
        }
      }
  
  type Activity = {
    Id : Uri
    Time : DateTimeOffset
    Label : string
    Users : Uri list
    Used : FileVersion list
    InformedBy : Uri seq
    }
  with static member fromCommit = function
    | Commit c,Commit c',r -> {
        Id = Uri.commit c r
        Time = c.Author.When
        Label = c.Message
        Users = [Uri.identity c]
        Used = FileVersion.fromDiff (diff (c,c') r)
        InformedBy = c.Parents |> Seq.map (fun c -> Uri ("commit:",short c r)) 
        }
      
module Main = 
  open Nessos.UnionArgParser
  open Git
  
  type Arguments =
    | Path of string
    | Since of string
  with interface IArgParserTemplate with
    member s.Usage =
      match s with
        | Path p -> "Path to a git repository"
        | Since r -> "Commit ref to generate PROV from"
        
  [<EntryPoint>]
  let main argv = 
    let parser = UnionArgParser.Create<Arguments>()
    let args = parser.Parse argv

    let c = repo (args.GetResult (<@ Path @>,defaultValue=".")) (commits (args.GetResult (<@ Since @>,defaultValue="HEAD")))
    printf "%A" c
    
    0// return an integer exit code

