module Prov 
  open Git
  open System

  let short sha = function
    | Repository r ->
      r.ObjectDatabase.ShortenObjectId sha
 
  type Uri =
    | Uri of string 
    with static member commit r c = Uri (sprintf "commit/%s" (short c r))
         static member workingarea = Uri "workingarea" 
         static member identity (c:LibGit2Sharp.Commit) = Uri (sprintf "user/%s" c.Author.Email)
         static member versionedcontent (c,p) = Uri (sprintf "commit/%s/%s" c p)
         static member individual (r,p) = Uri p
         override x.ToString () = match x with | Uri s -> sprintf "base:%s" s 
         
  type FileVersion = {
    Id : Uri
    Content: Git.Content
    PreviousVersion : string option
    SpecialisationOf : Uri
    Commit : Uri
    AttributedTo : Uri
    }
  with static member from (c:LibGit2Sharp.Commit,c':LibGit2Sharp.Commit,r) = seq {
        let d = diff (c.Tree,c'.Tree) r
        for f in d.Modified do yield {
          Id = Uri.versionedcontent (f.Oid.Sha,f.Path)
          Content = Git.content c.Id.Sha f.Path r
          PreviousVersion = Some f.OldOid.Sha 
          SpecialisationOf = Uri.individual (r,f.Path) 
          Commit = Uri.commit r c
          AttributedTo = Uri.identity c 
          }
        }
       static member from (wx:LibGit2Sharp.StatusEntry list,r) = seq {
         for f in wx do yield {
           Id = Uri f.FilePath
           Content = Git.Content.Text (System.IO.File.ReadAllText (f.FilePath))
           PreviousVersion = None
           SpecialisationOf = Uri.individual (r,f.FilePath)
           Commit = Uri.workingarea
           AttributedTo = Uri System.Environment.UserName
         }
       } 
  
  type Activity = {
    Id : Uri
    Time : DateTimeOffset
    Label : string
    User : Uri
    Used : FileVersion seq
    InformedBy : Uri list 
    }
  with static member fromCommit r = function
        | Commit c,Commit c' -> {
            Id = Uri.commit r c
            Time = c.Author.When
            Label = c.Message
            User = Uri.identity c
            Used = FileVersion.from (c,c',r)
            InformedBy = [for p in c.Parents -> Uri.commit r p]
          }
       static member fromWorkingArea r = function
        | WorkingArea (wx,Commit c) -> {
            Id = Uri.workingarea
            Time = DateTimeOffset.Now
            Label = "Uncommitted changes in working area"
            User = Uri System.Environment.UserName
            Used = FileVersion.from (wx,r)
            InformedBy = [Uri.commit r c]
          } 

