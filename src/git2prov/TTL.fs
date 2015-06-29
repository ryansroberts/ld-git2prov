module Translate

open common.RDF
open Prov
open Git
open VDS.RDF
open VDS.RDF.Writing

let provHistory (g : IGraph) (act : Activity) =
  let triples = triples g
  let puri = puri g
  let date = date g
  let a = a g
  let qn = qn g
  let suri = suri g
  let blank = blank g
  let literal = literal g
  triples (puri act.Id,
           [ (a, qn "prov:Activity")
             (qn "prov:startedAtTime", date act.Time)
             (qn "prov:endedAtTime", date act.Time)
             (qn "prov:wasAssociatedWith", puri act.User)
             (qn "rdfs:label", literal act.Label)
             (qn "prov:qualifiedAssociation",
              blank [ (a, qn "prov:Association")
                      (qn "prov:agent", puri act.User)
                      (qn "prov:hadRole", literal "author, committer") ]) ])
  for i in act.InformedBy do
    triples (puri act.Id, [ qn "prov:informedBy", puri i ])
  for u in act.Used do
    triples (puri act.Id, [ qn "prov:uses", puri u.Id ])
    triples (puri u.Id,
             [ (a, qn "prov:Entity")
               (qn "prov:wasGeneratedBy", puri u.Commit)
               (qn "prov:wasAttributedTo", puri u.AttributedTo)
               (qn "compilation:path", literal (string u.Path))
               (qn "prov:specializationOf", puri u.SpecialisationOf) ])
    match u.Content with
    | Working x -> triples (puri u.Id, [ (qn "compilation:content", suri x) ])
    | Revision x -> triples (puri u.Id, [ (qn "compilation:content", suri x) ])
  ()

let provCompilation (g : IGraph) (act : Activity) =
  let triples = triples g
  let puri = puri g
  let date = date g
  let a = a g
  let qn = qn g
  let blank = blank g
  let literal = literal g
  triples (puri act.Id,
            [ (a, qn "compilation:Compilation")
              (qn "prov:startedAtTime", date act.Time)
              (qn "prov:wasAssociatedWith", puri act.User)
              (qn "rdfs:label", literal act.Label)
              (qn "prov:qualifiedAssociation",
                blank [(a, qn "prov:Association")
                       (qn "prov:agent", puri act.User)
                       (qn "prov:hadRole", literal "initiator") ]) ])
  for u in act.Used do
        triples (puri act.Id, [ qn "prov:uses", puri u.Id ])
  for i in act.InformedBy do
        triples (puri act.Id, [qn "prov:informedBy", puri i ])
  ()


let sameAs (g:IGraph) (t : TreeFile) =
  let triples = triples g
  let puri = puri g
  let date = date g
  let a = a g
  let qn = qn g
  let blank = blank g
  let literal = literal g
  triples (puri t.Id,
           [qn "compilation:tree", literal t.Hash])
  ()

let ttl (s : System.IO.FileStream) g =
  let writer =  CompressingTurtleWriter(WriterCompressionLevel.High)
  use tw = new System.IO.StreamWriter(s)
  writer.Save(g,tw)
