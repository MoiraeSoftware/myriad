namespace Myriad
open System
open Fantomas
open System.IO
open FSharp.Compiler.SyntaxTree
open FsAst
open Argu
open Tomlyn
open System.Collections.Generic
open System.Diagnostics

module Implementation =
    let findPlugins (path: string) =
        let assembly = System.Reflection.Assembly.LoadFrom(path)

        let gens =
            [ for t in assembly.GetTypes() do
                if t.GetCustomAttributes(typeof<Myriad.Core.MyriadGeneratorAttribute>, true).Length > 0
                then yield t ]
        gens
        
    let getConfigHandler (verbose: bool) (config: Tomlyn.Model.TomlTable) =
        fun name ->
            if verbose then
                printfn $"CONFIG: %A{config}"
                printfn $"LOOKING FOR: %s{name}"
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

    let getConfig (configFile) =

        let configFile =
                configFile
                |> Option.defaultValue (Path.Combine(Environment.CurrentDirectory, "myriad.toml"))

        let configFileCnt = File.ReadAllText configFile
        let config = Toml.Parse(configFileCnt, configFile) |> Toml.ToModel
        config

module Main =
    open Implementation
    type Arguments =
        | [<Mandatory>] InputFile of string
        | OutputFile of string
        | ConfigFile of string
        | ConfigKey of string
        | Plugin of string
        | [<CustomCommandLine("--wait-for-debugger")>] WaitForDebugger
        | Verbose
        | [<EqualsAssignment;CustomCommandLine("--additionalparams")>] AdditionalParams of key:string * value:string
        | SelfGeneration
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputFile _ -> "specify a file to use as input."
                | OutputFile _ -> "Specify the file name that the generated code will be written to."
                | ConfigFile _ -> "Specify a TOML file to use as config."
                | ConfigKey _ -> "Specify a key in config that will be passed to the generators."
                | Plugin _ -> "Register an assembly plugin."
                | WaitForDebugger _ -> "Wait for the debugger to attach."
                | Verbose -> "Log verbose processing details."
                | AdditionalParams _ -> "Specify additional parameters."
                | SelfGeneration -> "Generate code for the input file at the end of the input file."

    [<EntryPoint>]
    let main argv =
        let parser = ArgumentParser.Create<Arguments>(programName = "myriad")

        try
            let results = parser.Parse argv
            let verbose = results.Contains Verbose

            if results.Contains WaitForDebugger then
                while not(Debugger.IsAttached) do
                  Threading.Thread.Sleep(100)
                Debugger.Break()

            let inputFile = results.GetResult InputFile
            let outputFile = results.TryGetResult OutputFile
            let configKey = results.TryGetResult ConfigKey
            let config = getConfig(results.TryGetResult ConfigFile)
            let additionalParams = results.GetResults AdditionalParams |> dict
            let selfGeneration = results.Contains SelfGeneration
            let plugins = results.GetResults Plugin

            if verbose then
                printfn "Plugins:"
                plugins |> List.iter (printfn "- '%s'")

            let generators =
                plugins
                |> List.collect findPlugins

            if verbose then
                printfn "Generators:"
                generators |> List.iter (fun t -> printfn $"- '%s{t.FullName}'")

            let runGenerator (inputFile: string) (genType: Type) =
                let instance = Activator.CreateInstance(genType) :?> Myriad.Core.IMyriadGenerator

                let configHandler = getConfigHandler verbose config

                if verbose then
                    printfn $"Executing: %s{genType.FullName}..."

                let result =
                    try
                        if instance.ValidInputExtensions |> Seq.contains (Path.GetExtension(inputFile))
                        then
                            let context = { Core.GeneratorContext.ConfigKey = configKey
                                            Core.GeneratorContext.ConfigGetter = configHandler
                                            Core.GeneratorContext.InputFilename = inputFile
                                            Core.GeneratorContext.AdditionalParameters = additionalParams }
                            Some (instance.Generate(context))
                        else None
                    with
                    | exc ->
                        // emit the module with exception text
                        let info = SynComponentInfoRcd.Create (Ident.CreateLong $"%s{genType.Name}Failure")
                        let pattern =
                            // intentionally generating invalid identifier name to fail the compilation
                            let name = LongIdentWithDots.CreateString "!CompilationError"
                            SynPatRcd.CreateLongIdent(name, [])
                        let letBinding =
                            { SynBindingRcd.Let with
                                  Pattern = pattern
                                  Expr = SynExpr.CreateConstString exc.Message }
                        let modulDecl = SynModuleDecl.CreateNestedModule(info, [SynModuleDecl.CreateLet [letBinding]])
                        let moduleOrNamespace = SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong "")
                        Some [ { moduleOrNamespace with IsRecursive = true; Declarations = [modulDecl] } ]

                if verbose then
                    printfn $"Result: '%A{result}'"

                result

            if verbose then
                printfn "Exec generators:"
                printfn $"Input Filename:\n:%A{inputFile}"

            let generated =
                generators
                |> List.choose (runGenerator inputFile)
                |> List.concat

            let parseTree =
                let filename =
                    if selfGeneration then
                        inputFile
                    else if outputFile.IsSome then
                        outputFile.Value
                    else failwith "Error: No OutputFile was included, and --selfgeneration was not specified."
                ParsedInput.CreateImplFile(
                    ParsedImplFileInputRcd.CreateFs(filename)
                        .AddModules generated)

            let cfg = { FormatConfig.FormatConfig.Default with StrictMode = true }
            let formattedCode = CodeFormatter.FormatASTAsync(parseTree, "myriad.fsx", [], None, cfg) |> Async.RunSynchronously

            let header =
               [   "//------------------------------------------------------------------------------"
                   "//        This code was generated by myriad."
                   "//        Changes to this file will be lost when the code is regenerated."
                   "//------------------------------------------------------------------------------"]

            let code =
                [   yield! header
                    formattedCode ]

            if selfGeneration then
                let headerStartingLine =
                    seq { yield! System.IO.File.ReadLines inputFile }
                    |> Seq.tryFindIndex (fun line -> line = header.[1])
                    |> Option.map (fun index -> index - 2)

                let tempFile = Path.GetTempFileName()
                let linesToKeep =
                    let inputCode = File.ReadLines(inputFile)
                    match headerStartingLine with
                    | Some headerStartingLine -> 
                        inputCode
                        |> Seq.take headerStartingLine
                    | None -> inputCode

                if verbose then
                    printfn $"Writing to temp file: '%A{tempFile}'"
                File.WriteAllLines(tempFile, seq{ yield! linesToKeep; yield! code} )
                if verbose then
                    printfn $"Removing input file: '%A{tempFile}'"
                File.Delete(inputFile)
                if verbose then
                    printfn $"Renaming temp file to input file: '%A{tempFile}' -> '%A{inputFile}'"
                File.Move(tempFile, inputFile)
            else
                match outputFile with
                | Some filename ->
                    File.WriteAllLines(filename, code)
                | None -> failwith "Error: No OutputFile was included, and --selfgeneration was not specified."

            if verbose then
                printfn "Generated Code:\n%A" code
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
