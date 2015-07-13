module Prov

open Git
open common.RDF
open System.Collections.Generic
let sha (s : string) =
  let alg = System.Security.Cryptography.SHA256.Create()
  alg.ComputeHash(System.Text.Encoding.UTF8.GetBytes s) |> ignore
  System.BitConverter.ToString(alg.Hash).Replace("-", "")

let short sha = function
  | Repository r -> r.ObjectDatabase.ShortenObjectId sha
let iso (date : System.DateTimeOffset) = date.ToString("o")
let (++) a b = System.IO.Path.Combine(a, b)


let d = Dictionary<_,_>()
let memo =
  (fun x f ->
    let r = ref []
    if(not(d.TryGetValue(x,r))) then f else !r)

let follow p r = memo p (follow p r)

let hashFor p r =
  let xs = follow (Path p) r
  if Seq.isEmpty xs then
    sha p
  else
    let (LogEntry l) = Seq.last xs
    sha l.Path


type Uri with
  static member commit r c = Uri("git2prov", [ "commit" ], Some(short c r))
  static member compilation c =
    Uri("git2prov", [ "compilation" ], Some(sha (string c)))
  static member workingarea = Uri("git2prov", [ "commit" ], Some(System.Environment.UserName + "-workingarea"))
  static member identity (c : LibGit2Sharp.Commit) =
    Uri("git2prov", [ "user" ], Some c.Author.Email)
  static member identity() =
    Uri("git2prov", [ "user" ], Some System.Environment.UserName)
  static member versionedcontent (Commit c) p r =
    Uri("git2prov", [ "entity" ], Some((short c r) + ":" + p))
  static member lastVersionAtPath (Commit c)  p r =
    let xl = follow (Path p) r
    if Seq.isEmpty xl then
      Uri("git2prov", [ "entity" ], Some((short c r) + ":" + p))
    else
      let (LogEntry l ) = xl |> Seq.head
      Uri("git2prov", [ "entity" ], Some((short l.Commit r) + ":" + p))
  static member workingAreaFile (f : LibGit2Sharp.StatusEntry) =
    Uri("git2prov", [ "workingarea" ], Some(sha f.FilePath))
  static member specialisationOf (r,p) =
    Uri("ld", [ "resource" ], Some (hashFor p r))

and TreeFile = {
    Id : Uri
    Path : Path
    SpecialisationOf : Uri
    Tree : Uri
  } with
  static member from r (Commit c) =
    let (Tree t) = tree (Commit c)
    let rec iterT (t:LibGit2Sharp.Tree) = seq {
      for t' in t do
      match t'.TargetType with
      | LibGit2Sharp.TreeEntryTargetType.Blob -> yield t'
      | LibGit2Sharp.TreeEntryTargetType.Tree -> yield! iterT (t'.Target :?> LibGit2Sharp.Tree)
      | _ -> ()
    }
    [for f in (iterT t) do
     yield {
       Id = Uri.lastVersionAtPath (Commit c) (f.Path) r
       Path = Path(f.Path)
       SpecialisationOf = Uri.specialisationOf (r, f.Path)
       Tree = Uri.commit r c}]

  static member fromWorkingArea r = function
    | WorkingArea(wx, Commit c) -> [
      let working = [for w in wx do
                       yield {Id = Uri.workingAreaFile w
                              Path = Path(w.FilePath)
                              SpecialisationOf = Uri.specialisationOf (r, w.FilePath)
                              Tree = Uri.workingarea}]
      let workingPaths = Set.ofList (working |> List.map(fun w -> w.Path))
      let notChangedInWorking f = not(Set.contains f.Path workingPaths)
      yield! working
      yield! TreeFile.from r (Commit c)
             |> Seq.filter notChangedInWorking
     ]

type FileVersion =
  { Id : Uri
    Content : Git.Content
    Path : Path
    PreviousVersion : string option
    SpecialisationOf : Uri
    Time : System.DateTimeOffset
    Commit : Uri
    AttributedTo : Uri } with
  static member from (Commit c, r) =
    [let d = diff (Commit c) r
     for f in Seq.concat [ d.Modified; d.Added; d.Renamed; d.Copied ] do
        yield { Id = Uri.versionedcontent (Commit c) (f.Path) r
                Content = Git.content (short c r) (f.Path) r
                Path = Path(f.Path)
                PreviousVersion = Some f.OldOid.Sha
                SpecialisationOf = Uri.specialisationOf (r,f.Path)
                Commit = Uri.commit r c
                Time = c.Author.When
                AttributedTo = Uri.identity c }]

  static member from (wx : LibGit2Sharp.StatusEntry list, r) =
    seq {
      for f in wx do
        yield { Id = Uri.workingAreaFile f
                Content = Git.unstagedContent f r
                Path = Path(f.FilePath)
                PreviousVersion = None
                SpecialisationOf = Uri.specialisationOf (r, f.FilePath)
                Commit = Uri.workingarea
                Time = System.DateTimeOffset.Now
                AttributedTo = Uri.identity() }
      }

type Activity =
  { Id : Uri
    Time : System.DateTimeOffset
    Hash : string option
    Label : string
    User : Uri
    Used : FileVersion seq
    InformedBy : Uri seq }
  static member fromCommit r = function
    | Commit c ->
      { Id = Uri.commit r c
        Hash = Some (short c r)
        Time = c.Author.When
        Label = c.Message
        User = Uri.identity c
        Used = FileVersion.from (Commit c, r)
        InformedBy =
          [ for p in c.Parents -> Uri.commit r p ] }
  static member fromWorkingArea r = function
    | WorkingArea(wx, Commit c) ->
      { Id = Uri.workingarea
        Hash = None
        Time = System.DateTimeOffset.Now
        Label = "Uncommitted changes from working area"
        User = Uri.identity()
        Used = FileVersion.from (wx, r)
        InformedBy = [ Uri.commit r c ] }
  static member compilation a =
      {a with
        Id = Uri.compilation (string a.Id)
        Time = System.DateTimeOffset.Now
        InformedBy = [a.Id]
      }
