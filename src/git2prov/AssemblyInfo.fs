namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("git2prov")>]
[<assembly: AssemblyProductAttribute("git2prov")>]
[<assembly: AssemblyDescriptionAttribute("Translates git repository history into W3C PROV")>]
[<assembly: AssemblyVersionAttribute("1.2")>]
[<assembly: AssemblyFileVersionAttribute("1.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.2"
