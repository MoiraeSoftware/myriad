// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api
// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "myriad"
let summary = ""
let authors = "7sharp9"
let copyright = "Dave Thomas"
let tags = "fsharp;codegen;generation;metaprogramming"
let gitOwner = "MoiraeSoftware"
let gitName = "myriad"
let gitHome = "https://github.com/" + gitOwner
let gitUrl = gitHome + "/" + gitName

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
let isNullOrWhiteSpace = System.String.IsNullOrWhiteSpace

let exec cmd args dir =
    let proc =
        CreateProcess.fromRawCommandLine cmd args
        |> CreateProcess.ensureExitCodeWithMessage (sprintf "Error while running '%s' with args: %s" cmd args)
    (if isNullOrWhiteSpace dir then proc
    else proc |> CreateProcess.withWorkingDirectory dir)
    |> Proc.run
    |> ignore

let getBuildParam = Environment.environVarOrDefault
let DoNothing = ignore

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let nugetDir  = "./bin/nupkg"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let changelogFilename = "CHANGELOG.md"
let changelog = Changelog.load changelogFilename
let latestEntry = changelog.LatestEntry

let configuration =
    //Use Release as deafult
    match getBuildParam "Configuration" "Release" with
    | "Debug" -> DotNet.BuildConfiguration.Debug
    | "Release" -> DotNet.BuildConfiguration.Release
    | config -> DotNet.BuildConfiguration.Custom config


// --------------------------------------------------------------------------------------
// Build Targets
// --------------------------------------------------------------------------------------

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [nugetDir]
)

Target.create "Build" (fun _ ->
    //Build Myriad first to build Myriad engine, than build whole solution (including tests)
    DotNet.build
        (fun c -> {c with Configuration = configuration})
        "src/Myriad/Myriad.fsproj"

    DotNet.build
        (fun c -> {c with Configuration = configuration})
        "src/Myriad.sln"
)

//Build release version that can be used as an input for documentation genration
Target.create "BuildForDocs" (fun _ ->
    //Build Myriad first to build Myriad engine, than build whole solution (including tests)
    DotNet.build
        (fun c -> {
            c with
                OutputPath = Some "./temp/"
                Configuration =  DotNet.BuildConfiguration.Release})
        "src/Myriad/Myriad.fsproj"

    DotNet.build
        (fun c -> {
            c with
                OutputPath = Some "./temp/"
                Configuration =  DotNet.BuildConfiguration.Release})
        "src/Myriad.sln"
)

Target.create "Pack" (fun _ ->
    let properties = [
        ("Version", latestEntry.NuGetVersion);
        ("Authors", authors)
        ("PackageProjectUrl", gitUrl)
        ("PackageTags", tags)
        ("RepositoryType", "git")
        ("RepositoryUrl", gitUrl)
        ("PackageLicenseExpression", "Apache-2.0")
        ("Copyright", copyright)
        ("PackageReleaseNotes", sprintf "%s/blob/v%s/CHANGELOG.md" gitUrl latestEntry.NuGetVersion)
        ("EnableSourceLink", "true")
    ]

    //Pack SDK
    DotNet.pack (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            OutputPath = Some nugetDir
            MSBuildParams = { p.MSBuildParams with Properties = properties }
        }
    ) "src/Myriad.Sdk/Myriad.Sdk.proj"

    //Pack whole solution
    DotNet.pack (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            OutputPath = Some nugetDir
            MSBuildParams = { p.MSBuildParams with Properties = properties }
        }
    ) "src/Myriad.sln"

    let cmd =
        sprintf "dotnet-mergenupkg --source \"%s/Myriad.Sdk.%s.nupkg\" --other \"%s/Myriad.%s.nupkg\" --tools --only-files"
            nugetDir latestEntry.NuGetVersion nugetDir latestEntry.NuGetVersion

    exec "dotnet" cmd "."
)

Target.create "Test" (fun _ ->
    let cmd = sprintf  @"run --project .\test\Myriad.IntegrationPluginTests\Myriad.IntegrationPluginTests.fsproj -c %s" (configuration.ToString())
    exec "dotnet" cmd  "."
)

Target.create "Docs" (fun _ ->
  Shell.cleanDirs ["docs\\_public";]
  exec "dotnet"  @"fornax build" "docs"
)


Target.create "Default" DoNothing

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "Default"

"Clean"
  ==> "Build"
  ==> "BuildForDocs"
  ==> "Docs"

"Build"
  ==> "Pack"

Target.runOrDefault "Default"