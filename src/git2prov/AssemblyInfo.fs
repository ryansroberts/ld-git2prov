namespace System

open System.Reflection

[<assembly:AssemblyTitleAttribute("git2prov")>]
[<assembly:AssemblyProductAttribute("git2prov")>]
[<assembly:AssemblyDescriptionAttribute("Translates git repository history into W3C PROV")>]
[<assembly:AssemblyVersionAttribute("1.0")>]
[<assembly:AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation = 
    [<Literal>]
    let Version = "1.0"
