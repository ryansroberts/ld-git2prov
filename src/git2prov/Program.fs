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
    with static member commit r c = Uri ("commit:",short c r)
         static member identity (c:LibGit2Sharp.Commit) = Uri ("individual:",c.Author.Email)
         static member versionedcontent c = Uri ("versionedcontent:",c)
         static member individual (r,f:LibGit2Sharp.TreeEntryChanges) = Uri ("individual:",f.Path)
         
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
      Id = Uri.versionedcontent f.Oid.Sha  
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
    Users : Uri list
    Used : FileVersion seq
    InformedBy : Uri seq
    }
  with static member fromCommit = function
    | Commit c,Commit c',r -> {
        Id = Uri.commit r c
        Time = c.Author.When
        Label = c.Message
        Users = [Uri.identity c]
        Used = FileVersion.from c c' r
        InformedBy = c.Parents |> Seq.map (Uri.commit r)
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

