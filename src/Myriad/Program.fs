namespace Myriad
open System
open Fantomas
open System.IO
open FSharp.Compiler.SyntaxTree
open FsAst
open Argu
open Tomlyn
open System.Collections.Generic

module Main =

    type Arguments =
        | [<Mandatory>] InputFile of string
        | [<Mandatory>] OutputFile of string
        | ConfigFile of string
        | Plugin of string
        | [<CustomCommandLine("--wait-for-debugger")>] WaitForDebugger
        | Verbose
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputFile _ -> "specify a file to use as input."
                | OutputFile _ -> "Specify the file name that the generated code will be written to."
                | ConfigFile _ -> "Specify a TOML file to use as config."
                | Plugin _ -> "Register an assembly plugin."
                | WaitForDebugger _ -> "Wait for the debugger to attach."
                | Verbose -> "Log verbose processing details."

    [<EntryPoint>]
    let main argv =
        let parser = ArgumentParser.Create<Arguments>(programName = "myriad")

        try
            let results = parser.Parse argv

            let verbose = results.Contains Verbose

            match results.TryGetResult WaitForDebugger with
            | None -> ()
            | Some _ ->
                while not(Diagnostics.Debugger.IsAttached) do
                  System.Threading.Thread.Sleep(100)
                System.Diagnostics.Debugger.Break()

            let inputFile = results.GetResult InputFile
            let outputFile = results.GetResult OutputFile
            let configFile =
                results.TryGetResult ConfigFile
                |> Option.defaultValue (Path.Combine(Environment.CurrentDirectory, "myriad.toml"))

            let configFileCnt = File.ReadAllText configFile
            let config = Tomlyn.Toml.Parse(configFileCnt, configFile) |> Toml.ToModel

            let plugins = results.GetResults Plugin

            if verbose then
                printfn "Plugins:"
                plugins |> List.iter (printfn "- '%s'")

            let findPlugins (path: string) =
                let assembly = System.Reflection.Assembly.LoadFrom(path)

                let gens =
                    [ for t in assembly.GetTypes() do
                        if t.GetCustomAttributes(typeof<Myriad.Core.MyriadGeneratorAttribute>, true).Length > 0
                        then yield t ]
                gens

            let generators =
                plugins
                |> List.collect findPlugins

            if verbose then
                printfn "Generators:"
                generators |> List.iter (fun t -> printfn "- '%s'" t.FullName)

            let runGenerator (inputFile: string) (genType: Type) =
                let instance = Activator.CreateInstance(genType) :?> Myriad.Core.IMyriadGenerator

                let configHandler =
                    fun name ->
                        printfn "CONFIG %A" config
                        printfn "LOOKING FOR: %s" name
                        match config.TryGetToml name with
                        | true, x when x.Kind = Model.ObjectKind.Table ->
                            try
                                let x = (x :?> Model.TomlTable)
                                let x = (x :> IDictionary<string,obj>)
                                x |> Seq.map (|KeyValue|)
                            with
                            | _ ->
                                printfn "FAIL !"
                                Seq.empty
                        | _ ->
                            printfn "FAIL @"
                            Seq.empty

                if verbose then
                    printfn "Executing: %s..." genType.FullName

                let result =
                    try
                        if instance.ValidInputExtensions |> Seq.contains (Path.GetExtension(inputFile))
                        then Some (instance.Generate(configHandler, inputFile))
                        else None
                    with
                    | exc ->
                        // emit the module with exception text
                        let info = SynComponentInfoRcd.Create (Ident.CreateLong (sprintf "%sFailure" genType.Name))
                        let pattern =
                            // intentionally generating invalid identifier name to fail the compilation
                            let name = LongIdentWithDots.CreateString "!CompilationError"
                            SynPatRcd.CreateLongIdent(name, [])
                        let letBinding =
                            { SynBindingRcd.Let with
                                  Pattern = pattern
                                  Expr = SynExpr.CreateConstString exc.Message }
                        let modulDecl = SynModuleDecl.CreateNestedModule(info, [SynModuleDecl.CreateLet [letBinding]])
                        let ns = Ident.CreateLong "" //TODO
                        let moduleOrNamespace = SynModuleOrNamespaceRcd.CreateNamespace(ns)
                        Some [ { moduleOrNamespace with IsRecursive = true; Declarations = [modulDecl] } ]

                if verbose then
                    printfn "Result: '%A'" result

                result

            if verbose then
                printfn "Exec generators:"

            if verbose then
                printfn "Input Filename:\n:%A" inputFile

            let generated =
                generators
                |> List.choose (runGenerator inputFile)
                |> List.concat

            let parseTree =
                ParsedInput.CreateImplFile(
                    ParsedImplFileInputRcd.CreateFs(outputFile)
                        .AddModules generated)

            let cfg = { FormatConfig.FormatConfig.Default with StrictMode = true } 
            let formattedCode = CodeFormatter.FormatASTAsync(parseTree, "myriad.fsx", [], None, cfg) |> Async.RunSynchronously

            let code =
                [   "//------------------------------------------------------------------------------"
                    "//        This code was generated by myriad."
                    "//        Changes to this file will be lost when the code is regenerated."
                    "//------------------------------------------------------------------------------"
                    formattedCode ]
                |> String.concat Environment.NewLine

            File.WriteAllText(outputFile, code)

            printfn "%A" code

            if verbose then
                printfn "AST-----------------------------------------------"
                printfn "%A" parseTree
            0 // return an integer exit code

        with
        | :? ArguParseException as ae ->
            printfn "%s" ae.Message
            match ae.ErrorCode with
            | Argu.ErrorCode.HelpText -> 0
            | _ -> 2
        | :? ArguParseException as ae when ae.ErrorCode = Argu.ErrorCode.HelpText ->
            printfn "%s" ae.Message
            3
        | :? FileNotFoundException as fnf ->
            printfn "ERROR: inputfile %s doesn not exist\n%s" fnf.FileName (parser.PrintUsage())
            4
