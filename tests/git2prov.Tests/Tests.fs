module Tests

open Xunit
open System.Diagnostics
open Nessos.UnionArgParser
open Main
open System.IO
open common.RDF
open TestSupport



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
let ``Changes from HEAD to hash of previous commit``() =
    clone "testrepo"
    g2p [ Main.Path "testrepo"
          ShowHistory
          IncludeWorkingArea
          Since "all" ]
