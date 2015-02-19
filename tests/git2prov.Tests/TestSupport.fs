module TestSupport
 
open common.RDF
open System
open VDS.RDF
open VDS.RDF.Writing.Formatting
open Xunit
open System.Diagnostics
open Nessos.UnionArgParser
open Main
open System.IO
open common.RDF


(* Spooged from csharp, so looks bad *)
let showDifferences (report:VDS.RDF.GraphDiffReport) = [
    let formatter = NTriplesFormatter()

    if (report.AreEqual) then
        yield("Graphs are Equal")
        yield ""
        yield("Blank Node Mapping between Graphs:")
        for kvp in report.Mapping do
          yield(kvp.Key.ToString(formatter) + " => " + kvp.Value.ToString(formatter))
    else
        yield("Graphs are non-equal")
        yield ""
        yield("Triples added to 1st Graph to give 2nd Graph:")
        for t in report.AddedTriples do
          yield(t.ToString(formatter))
        yield ""
        yield("Triples removed from 1st Graph to given 2nd Graph:")
        for t in report.RemovedTriples do
            yield(t.ToString(formatter))
        yield ""
        yield("Blank Node Mapping between Graphs:")
        for kvp in report.Mapping do
            yield(( kvp.Key.ToString(formatter) ) + " => " + ( kvp.Value.ToString(formatter) ))
        yield ""
        yield("MSGs added to 1st Graph to give 2nd Graph:")
        for msg in report.AddedMSGs do
            for t in msg.Triples do
                yield(t.ToString(formatter))
            yield ""
        yield ""
        yield("MSGs removed from 1st Graph to give 2nd Graph:")
        for msg in report.RemovedMSGs do
            for t in msg.Triples do
                yield(t.ToString(formatter))
            yield ""
  ]
    
let pathToExpectations = "../tests/git2prov.Tests/expected/"
let approveGraph (expectedName:string) (ttl:System.IO.Stream) =
  let g = Store.loadFile (pathToExpectations ++ (sprintf "%s.ttl" expectedName))
  let g' = Store.loadTtl ttl

  let diff = Store.diff g g'
  Assert.True(diff.AreEqual ,showDifferences diff |> String.concat Environment.NewLine)

let clone repo = 
    if Directory.Exists repo then Directory.Delete(repo, true)
    let ps = 
        Process.Start
            ("git", sprintf "clone ../tests/git2prov.Tests/examples/%s" repo)
    ps.WaitForExit()


