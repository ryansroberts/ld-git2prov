module Prov

open Git
open System

let short sha = function 
    | Repository r -> r.ObjectDatabase.ShortenObjectId sha
let iso (date : DateTimeOffset) = date.ToString("o")

type Uri = 
    | Uri of string
    static member commit r c = Uri(sprintf "git2prov:commit/%s" (short c r))
    static member compilation = 
        Uri
            (sprintf "compilation:compilation/%s" 
                 (iso System.DateTimeOffset.Now))
    static member workingarea = Uri "git2prov:workingarea"
    static member identity (c : LibGit2Sharp.Commit) = 
        Uri(sprintf "git2prov:user/%s" c.Author.Email)
    static member identity() = 
        Uri(sprintf "git2prov:user/%s" System.Environment.UserName)
    static member versionedcontent (c, p) = 
        Uri(sprintf "git2prov:commit/%s/%s" c p)
    static member specialisationOf (r, p) = Uri(sprintf "base:%s" p)
    override x.ToString() = 
        match x with
        | Uri s -> s

type FileVersion = 
    { Id : Uri
      Content : Git.Content
      PreviousVersion : string option
      SpecialisationOf : Uri
      Commit : Uri
      AttributedTo : Uri }
    
    static member from (c : LibGit2Sharp.Commit, c' : LibGit2Sharp.Commit, r) = 
        seq { 
            let d = diff (c.Tree, c'.Tree) r
            for f in d.Modified do
                yield { Id = Uri.versionedcontent (f.Oid.Sha, f.Path)
                        Content = Git.content c.Id.Sha f.Path r
                        PreviousVersion = Some f.OldOid.Sha
                        SpecialisationOf = Uri.specialisationOf (r, f.Path)
                        Commit = Uri.commit r c
                        AttributedTo = Uri.identity c }
        }
    
    static member from (wx : LibGit2Sharp.StatusEntry list, r) = 
        seq { 
            for f in wx do
                yield { Id = Uri f.FilePath
                        Content = 
                            Git.Content.Text
                                (System.IO.File.ReadAllText(f.FilePath))
                        PreviousVersion = None
                        SpecialisationOf = Uri.specialisationOf (r, f.FilePath)
                        Commit = Uri.workingarea
                        AttributedTo = Uri System.Environment.UserName }
        }

type Activity = 
    { Id : Uri
      Time : DateTimeOffset
      Label : string
      User : Uri
      Used : FileVersion seq
      InformedBy : Uri list }
    static member fromCommit r = function 
        | Commit c, Commit c' -> 
            { Id = Uri.commit r c
              Time = c.Author.When
              Label = c.Message
              User = Uri.identity c
              Used = FileVersion.from (c, c', r)
              InformedBy = 
                  [ for p in c.Parents -> Uri.commit r p ] }
    static member fromWorkingArea r = function 
        | WorkingArea(wx, Commit c) -> 
            { Id = Uri.workingarea
              Time = DateTimeOffset.Now
              Label = "Uncommitted changes from working area"
              User = Uri.identity()
              Used = FileVersion.from (wx, r)
              InformedBy = [ Uri.commit r c ] }
    static member concat ax = 
        { Id = Uri.compilation
          Time = DateTimeOffset.Now
          Label = "Compilation message that is actualy useful"
          User = Uri.identity()
          Used = 
              ax
              |> Seq.map (fun a -> a.Used)
              |> Seq.concat
              |> Seq.groupBy (fun a -> a.SpecialisationOf)
              |> Seq.map (fun (_, dx) -> dx |> Seq.last)
          InformedBy = [] }
