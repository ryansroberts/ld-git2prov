module Translate

open common.RDF
open Prov
open Git
open VDS.RDF
open VDS.RDF.Writing

let provHistory includeContent (g : IGraph) (act : Activity) = 
    let triples = triples g
    let puri = puri g
    let date = date g
    let a = a g
    let qn = qn g
    let blank = blank g
    let literal = literal g
    triples (puri act.Id, 
             [ (a, qn "prov:Activity")
               (qn "prov:startedAtTime", date act.Time)
               (qn "prov:endedAtTime", date act.Time)
               (qn "prov:wasAssociatedWith", puri act.User)
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
                   (qn "prov:specializationOf", puri u.SpecialisationOf) ])
        match u.Content, includeContent with
        | Content.Text t, true -> 
            triples (puri u.Id, [ (qn "cnt:chars", literal t) ])
        | _, _ -> ()
    g

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
               (qn "prov:qualifiedAssociation", 
                blank [ (a, qn "prov:Association")
                        (qn "prov:agent", puri act.User)
                        (qn "prov:hadRole", literal "initiator") ]) ])
    for u in act.Used do
        triples (puri act.Id, [ qn "prov:uses", puri u.Id ])
    g

let ttl (tw : System.IO.TextWriter) g = 
    let writer = 
        CompressingTurtleWriter(CompressionLevel = 3, PrettyPrintMode = true)
    writer.Save(g, tw)
