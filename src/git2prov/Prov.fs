module Prov

open Git
open common.RDF

let short sha = function 
    | Repository r -> r.ObjectDatabase.ShortenObjectId sha
let iso (date : System.DateTimeOffset) = date.ToString("o")
let (++) a b = System.IO.Path.Combine(a, b)

type Uri with
    static member commit r c = Uri("git2prov", [ "commit" ], Some(short c r))
    static member compilation = 
        Uri
            ("compilation", [ "compilation" ], 
             Some(iso System.DateTimeOffset.Now))
    static member workingarea = Uri("git2prov", [ "workingarea" ], None)
    static member identity (c : LibGit2Sharp.Commit) = 
        Uri("git2prov", [ "user" ], Some c.Author.Email)
    static member identity() = 
        Uri("git2prov", [ "user" ], Some System.Environment.UserName)
    static member versionedcontent (f : LibGit2Sharp.TreeEntryChanges) = 
        Uri("git2prov", [ "commit"; f.Oid.Sha ], Some f.Path)
    static member workingAreaFile (f : LibGit2Sharp.StatusEntry) = 
        Uri("git2prov", [ "workingarea" ], Some f.FilePath)
    static member specialisationOf (r, p) = Uri("base", [ p ], None)

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
                yield { Id = Uri.versionedcontent f
                        Content = Git.content c.Id.Sha f.Path r
                        PreviousVersion = Some f.OldOid.Sha
                        SpecialisationOf = Uri.specialisationOf (r, f.Path)
                        Commit = Uri.commit r c
                        AttributedTo = Uri.identity c }
        }
    
    static member from (wx : LibGit2Sharp.StatusEntry list, r) = 
        seq { 
            for f in wx do
                yield { Id = Uri.workingAreaFile f
                        Content = Git.unstagedContent f r
                        PreviousVersion = None
                        SpecialisationOf = Uri.specialisationOf (r, f.FilePath)
                        Commit = Uri.workingarea
                        AttributedTo = Uri.identity() }
        }

type Activity = 
    { Id : Uri
      Time : System.DateTimeOffset
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
              Time = System.DateTimeOffset.Now
              Label = "Uncommitted changes from working area"
              User = Uri.identity()
              Used = FileVersion.from (wx, r)
              InformedBy = [ Uri.commit r c ] }
    static member concat ax = 
        { Id = Uri.compilation
          Time = System.DateTimeOffset.Now
          Label = 
              "Change this to a compilation message that is actualy useful to somebody"
          User = Uri.identity()
          Used = 
              ax
              |> Seq.map (fun a -> a.Used)
              |> Seq.concat
              |> Seq.groupBy (fun a -> a.SpecialisationOf)
              |> Seq.map (fun (_, dx) -> dx |> Seq.last)
          InformedBy = [] }
