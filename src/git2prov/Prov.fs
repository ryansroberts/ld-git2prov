module Prov

open Git
open common.RDF

let sha (s : string) =
  let alg = System.Security.Cryptography.SHA256.Create()
  alg.ComputeHash(System.Text.Encoding.UTF8.GetBytes s) |> ignore
  System.BitConverter.ToString(alg.Hash).Replace("-", "")

let short sha = function
  | Repository r -> r.ObjectDatabase.ShortenObjectId sha
let iso (date : System.DateTimeOffset) = date.ToString("o")
let (++) a b = System.IO.Path.Combine(a, b)

type Uri with
  static member commit r c = Uri("git2prov", [ "commit" ], Some(short c r))
  static member compilation c =
    Uri("git2prov", [ "compilation" ], Some(sha (string c)))
  static member workingarea = Uri("git2prov", [ "commit" ], Some("workingarea"))
  static member identity (c : LibGit2Sharp.Commit) =
    Uri("git2prov", [ "user" ], Some c.Author.Email)
  static member identity() =
    Uri("git2prov", [ "user" ], Some System.Environment.UserName)
  static member versionedcontent hash p r =
    Uri("git2prov", [ "entity" ], Some((short hash r) + ":" + p))
  static member workingAreaFile (f : LibGit2Sharp.StatusEntry) =
    Uri("git2prov", [ "workingarea" ], Some(sha f.FilePath))
  static member specialisationOf (r, p) = Uri("ld", [ "resource" ], Some(sha p))


type TreeFile = {
    Id : Uri
    Path : Path
    SpecialisationOf : Uri
    Hash : string
  } with
  static member from r (Commit c) =
    let (Tree t) = tree (Commit c)
    seq {
        for f in t do
        yield {
        Id = Uri.versionedcontent c (f.Path) r
        Path = Path(f.Path)
        SpecialisationOf = Uri.specialisationOf (r, f.Path)
        Hash = (short c r)
        }
    }

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
    seq {
      let d = diff (Commit c) r
      for f in Seq.concat [ d.Modified; d.Added; d.Renamed; d.Copied ] do
        yield { Id = Uri.versionedcontent c (f.Path) r
                Content = Git.content (short c r) (f.Path) r
                Path = Path(f.Path)
                PreviousVersion = Some f.OldOid.Sha
                SpecialisationOf = Uri.specialisationOf (r, f.Path)
                Commit = Uri.commit r c
                Time = c.Author.When
                AttributedTo = Uri.identity c }
    }

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
    Label : string
    User : Uri
    Used : FileVersion seq
    InformedBy : Uri seq }
  static member fromCommit r = function
    | Commit c ->
      { Id = Uri.commit r c
        Time = c.Author.When
        Label = c.Message
        User = Uri.identity c
        Used = FileVersion.from (Commit c, r)
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
    let informed = ax
                    |> Seq.map (fun x -> x.Id)
                    |> Seq.toArray
    let used = ax
                |> Seq.map (fun a -> a.Used)
                |> Seq.concat
                |> Seq.sortBy (fun a -> a.Time)
                |> Seq.groupBy (fun a -> a.SpecialisationOf)
                |> Seq.map (fun (_, dx) -> dx |> Seq.last)
    { Id = Uri.compilation (informed.[informed.Length - 1])
      Time = System.DateTimeOffset.Now
      Label =
        "Change this to a compilation message that is actualy useful to somebody"
      User = Uri.identity()
      Used = used
      InformedBy = informed}
