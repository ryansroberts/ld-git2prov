module Tests

open Xunit
open System.Diagnostics
open Nessos.UnionArgParser
open Main
open System.IO
open common.RDF

let pathToExpectations = "../tests/git2prov.tests/expected/"
let approveGraph (expectedName:string) (ttl:System.IO.Stream) =
  let g = Store.loadFile (pathToExpectations ++ (sprintf "%s.ttl" expectedName))
  let g' = Store.loadTtl ttl

  let diff = Store.diff g g'
  Assert.True (diff.AreEqual,string diff)

let clone repo = 
    if Directory.Exists repo then Directory.Delete(repo, true)
    let ps = 
        Process.Start
            ("git", sprintf "clone ../tests/git2prov.Tests/examples/%s" repo)
    ps.WaitForExit()

let g2p args = 
    let parser = UnionArgParser.Create<Arguments>()
    let args = parser.PrintCommandLine args |> String.concat "  "
    let psi = 
        ProcessStartInfo
            (FileName = "git2prov.exe", UseShellExecute = false, 
             Arguments = args, RedirectStandardOutput = true)
    let ps = Process.Start psi
    ps.StandardOutput.BaseStream

[<Fact>]
let ``There are no changes from HEAD to HEAD``() = 
    clone "testrepo"
    g2p [ Main.Path "testrepo"
          ShowHistory
          Since "HEAD" ]
    |> approveGraph "HEADtoHEAD"

[<Fact>]
let ``Changes from HEAD to hash of previous commit``() = 
    clone "testrepo"
    g2p [ Main.Path "testrepo"
          ShowHistory
          Since "be3563a" ]
    |> approveGraph "HEADtobe3"

[<Fact>]
let ``Changes from HEAD to alias of previous commit``() = 
    clone "testrepo"
    g2p [ Main.Path "testrepo"
          ShowHistory
          Since "HEAD~1" ]
    |> approveGraph "HEADtoHEAD-1"

[<Fact>]
let ``Changes for all history``() = 
    clone "testrepo"
    g2p [ Main.Path "testrepo"
          ShowHistory
          Since "8496071"]
    |> approveGraph "AllHistory"
