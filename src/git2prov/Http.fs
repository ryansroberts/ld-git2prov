module Http

open Git
open common.RDF
open Suave
open Suave.Web
open Suave.Http
open Suave.Http.Successful
open Suave.Http.RequestErrors
open Suave.Http.Applicatives
open Suave.Types

let parts = function 
    | Repository r -> 
        (*Match file version entities and return content*)
        let content : WebPart = 
            urlScan "/%s/tree/%s/commit/%s/%s" (fun (rn, b, c, p) -> 
                let b = r.Branches.[b]
                OK "")
        choose [ content
                 NOT_FOUND "No handler" ]

let serve r p = 
    parts r 
    |> startWebServer 
           { defaultConfig with bindings = 
                                    [ HttpBinding.mk' HTTP ("0.0.0.0") p ]
                                listenTimeout = 
                                    System.TimeSpan.FromMilliseconds 2000. }
