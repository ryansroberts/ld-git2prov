// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO


// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------
// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"
// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "git2prov"
// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Translates git repository history into W3C PROV"
// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "Project has no description; update build.fsx"
// List of author names (for NuGet package)
let authors = [ "NICE" ]
// Tags for your project (for NuGet package)
let tags = ""
// File system information
let solutionFile = "git2prov.sln"
// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "bin/*Tests*.dll"
// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "nhsevidence"
let gitHome = "https://github.com/" + gitOwner
// The name of the project on GitHub
let gitName = "ld-git2prov"
// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/nhsevidence"
// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------
// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"

let genFSAssemblyInfo (projectPath) =
  let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
  let basePath = "src/" + projectName
  let fileName = basePath + "/AssemblyInfo.fs"
  CreateFSharpAssemblyInfo fileName
    [ Attribute.Title(projectName)
      Attribute.Product project
      Attribute.Description summary
      Attribute.Version release.AssemblyVersion
      Attribute.FileVersion release.AssemblyVersion ]

let genCSAssemblyInfo (projectPath) =
  let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
  let basePath = "src/" + projectName + "/Properties"
  let fileName = basePath + "/AssemblyInfo.cs"
  CreateCSharpAssemblyInfo fileName
    [ Attribute.Title(projectName)
      Attribute.Product project
      Attribute.Description summary
      Attribute.Version release.AssemblyVersion
      Attribute.FileVersion release.AssemblyVersion ]

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let fsProjs = !!"src/**/*.fsproj"
  let csProjs = !!"src/**/*.csproj"
  fsProjs |> Seq.iter genFSAssemblyInfo
  csProjs |> Seq.iter genCSAssemblyInfo)
// Build native
Target "Native"
  (fun _ ->
    (Shell.Exec
       ("sh", args = "-c './build.libgit2sharp.sh'",
        dir = FileUtils.pwd() + "/lib/libgit2sharp"))
    |> ignore
    !!"lib/libgit2sharp/libgit2/build/*"
    |> CopyFiles "bin")

// --------------------------------------------------------------------------------------
// Clean build results
Target "Clean" (fun _ -> CleanDirs [ "bin"; "temp" ])
Target "CleanDocs" (fun _ -> CleanDirs [ "docs/output" ])
// --------------------------------------------------------------------------------------
// Build library & test project
Target "Build" (fun _ ->
  !!solutionFile
  |> MSBuildRelease "" "Rebuild"
  |> ignore)
// --------------------------------------------------------------------------------------
// Run the unit tests using test runner
Target "RunTests" (fun _ ->
  !!testAssemblies |> xUnit(fun p ->
                            { p with TimeOut = TimeSpan.FromMinutes 20. }))

Target "QuickTests" (fun _ ->
  !!testAssemblies |> xUnit(fun p ->
                        { p with TimeOut = TimeSpan.FromMinutes 20. }))

// Generate the documentation
Target "GenerateReferenceDocs"
  (fun _ ->
  if not
     <| executeFSIWithArgs "docs/tools" "generate.fsx"
          [ "--define:RELEASE"; "--define:REFERENCE" ] [] then
    failwith "generating reference documentation failed")

let generateHelp' fail debug =
  let args =
    if debug then [ "--define:HELP" ]
    else [ "--define:RELEASE"; "--define:HELP" ]
  if executeFSIWithArgs "docs/tools" "generate.fsx" args [] then
    traceImportant "Help generated"
  else if fail then failwith "generating help documentation failed"
  else traceImportant "generating help documentation failed"

let generateHelp fail = generateHelp' fail false

Target "GenerateHelp" (fun _ ->
  DeleteFile "docs/content/release-notes.md"
  CopyFile "docs/content/" "RELEASE_NOTES.md"
  Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"
  DeleteFile "docs/content/license.md"
  CopyFile "docs/content/" "LICENSE.txt"
  Rename "docs/content/license.md" "docs/content/LICENSE.txt"
  generateHelp true)
Target "GenerateHelpDebug" (fun _ ->
  DeleteFile "docs/content/release-notes.md"
  CopyFile "docs/content/" "RELEASE_NOTES.md"
  Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"
  DeleteFile "docs/content/license.md"
  CopyFile "docs/content/" "LICENSE.txt"
  Rename "docs/content/license.md" "docs/content/LICENSE.txt"
  generateHelp' true true)
Target "KeepRunning" (fun _ ->
  use watcher =
    new FileSystemWatcher(DirectoryInfo("docs/content").FullName, "*.*")
  watcher.EnableRaisingEvents <- true
  watcher.Changed.Add(fun e -> generateHelp false)
  watcher.Created.Add(fun e -> generateHelp false)
  watcher.Renamed.Add(fun e -> generateHelp false)
  watcher.Deleted.Add(fun e -> generateHelp false)
  traceImportant "Waiting for help edits. Press any key to stop."
  System.Console.ReadKey() |> ignore
  watcher.EnableRaisingEvents <- false
  watcher.Dispose())
Target "GenerateDocs" DoNothing

let createIndexFsx lang =
  let content = """(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../../bin"

(**
F# Project Scaffold ({0})
=========================
*)
"""
  let targetDir = "docs/content" @@ lang
  let targetFile = targetDir @@ "index.fsx"
  ensureDirectory targetDir
  System.IO.File.WriteAllText(targetFile, System.String.Format(content, lang))

Target "AddLangDocs" (fun _ ->
  let args = System.Environment.GetCommandLineArgs()
  if args.Length < 4 then failwith "Language not specified."
  args.[3..] |> Seq.iter (fun lang ->
                  if lang.Length <> 2 && lang.Length <> 3 then
                    failwithf
                      "Language must be 2 or 3 characters (ex. 'de', 'fr', 'ja', 'gsw', etc.): %s"
                      lang
                  let templateFileName = "template.cshtml"
                  let templateDir = "docs/tools/templates"
                  let langTemplateDir = templateDir @@ lang
                  let langTemplateFileName = langTemplateDir @@ templateFileName
                  if System.IO.File.Exists(langTemplateFileName) then
                    failwithf
                      "Documents for specified language '%s' have already been added."
                      lang
                  ensureDirectory langTemplateDir
                  Copy langTemplateDir [ templateDir @@ templateFileName ]
                  createIndexFsx lang))
// --------------------------------------------------------------------------------------
// Release Scripts
Target "ReleaseDocs" (fun _ ->
  let tempDocsDir = "temp/gh-pages"
  CleanDir tempDocsDir
  Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages"
    tempDocsDir
  CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
  StageAll tempDocsDir
  Git.Commit.Commit tempDocsDir
    (sprintf "Update generated documentation for version %s"
       release.NugetVersion)
  Branches.push tempDocsDir)

#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"

open Octokit

Target "Release" (fun _ ->
  StageAll ""
  Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
  Branches.push ""
  Branches.tag "" release.NugetVersion
  Branches.pushTag "" "origin" release.NugetVersion
  // release on github
  createClient (getBuildParamOrDefault "github-user" "")
    (getBuildParamOrDefault "github-pw" "")
  |> createDraft gitOwner gitName release.NugetVersion
       (release.SemVer.PreRelease <> None) release.Notes
  // TODO: |> uploadFile "PATH_TO_FILE"
  |> releaseDraft
                  |> Async.RunSynchronously)
Target "BuildPackage" (fun _ ->
  let n = Environment.GetEnvironmentVariable "DRONE_BUILD_NUMBER"
  let v = sprintf "1.2.%s" n
  [
   ("mono",".paket/paket.exe pack output . version " + v)
   ("mono",sprintf ".paket/paket.exe push apikey f3709835-b06d-444a-87dc-18786597f812 url https://www.nuget.org file NICE.git2prov.%s.nupkg" v)
  ]
  |> List.iter (fun (v,a) -> Shell.Exec (v,args=a) |> ignore)
)


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override
Target "All" DoNothing
"Clean" ==> "AssemblyInfo" ==> "Native" ==> "Build" ==> "RunTests"
=?> ("GenerateReferenceDocs", isLocalBuild && not isMono)
=?> ("GenerateDocs", isLocalBuild && not isMono) ==> "All"
=?> ("ReleaseDocs", isLocalBuild && not isMono)
"All"
==> "BuildPackage"

"CleanDocs" ==> "GenerateHelp" ==> "GenerateReferenceDocs" ==> "GenerateDocs"
"CleanDocs" ==> "GenerateHelpDebug"
"GenerateHelp" ==> "KeepRunning"
"ReleaseDocs" ==> "Release"
"BuildPackage" ==> "Release"
RunTargetOrDefault "All"
