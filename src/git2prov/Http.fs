module Http
  open Git
  open TTL
  open Suave
  open Suave.Web
  open Suave.Http
  open Suave.Http.Successful
  open Suave.Http.Applicatives
  open Suave.Types

  let parts r = function
    | Repository r ->
        (*Match file version entities and return content*)
        let content : WebPart = url_scan "/%s/tree/%s/commit/%s/%s" (fun (rn,b,c,p) ->
                                            let b = r.Branches.[b]
                                            OK ""
                                           )

        choose [content]
 
  let serve r p = ()
