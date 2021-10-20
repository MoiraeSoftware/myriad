namespace Myriad
open System
open Fantomas
open System.IO
open FSharp.Compiler.SyntaxTree
open Argu
open Myriad.Core
open Tomlyn
open System.Collections.Generic
open System.Diagnostics
open Myriad.Core.Ast
open FsAst
open Tomlyn.Model

module Implementation =
    let findPlugins (path: string) =
        let assembly = System.Reflection.Assembly.LoadFrom(path)

        let gens =
            [ for t in assembly.GetTypes() do
                if t.GetCustomAttributes(typeof<MyriadGeneratorAttribute>, true).Length > 0
                then yield t ]
        gens
        
    let getConfigHandler (verbose: bool) (config: Model.TomlTable) =
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
        | [<HiddenAttribute>] ContextFile of string
        | Plugin of string
        | [<CustomCommandLine("--wait-for-debugger")>] WaitForDebugger
        | Verbose
        | [<EqualsAssignment;CustomCommandLine("--additionalparams")>] AdditionalParams of key:string * value:string
        | InlineGeneration
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputFile _ -> "specify a file to use as input."
                | OutputFile _ -> "Specify the file name that the generated code will be written to."
                | ConfigFile _ -> "Specify a TOML file to use as config."
                | ConfigKey _ -> "Specify a key in config that will be passed to the generators."
                | ContextFile _ -> "Specify a context file for the generators to use."
                | Plugin _ -> "Register an assembly plugin."
                | WaitForDebugger _ -> "Wait for the debugger to attach."
                | Verbose -> "Log verbose processing details."
                | AdditionalParams _ -> "Specify additional parameters."
                | InlineGeneration -> "Generate code for the input file at the end of the input file."
        

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
            let inlineGeneration = results.Contains InlineGeneration
            let plugins = results.GetResults Plugin
            let contextFile = results.TryGetResult ContextFile
            
            let projectContext =
                match contextFile with
                | Some file when File.Exists file ->
                    let result = Toml.Parse(File.ReadAllText(file), file).ToModel()
                    let project = result.["project"] :?> String
                    let refs = result.["referencePaths"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    let compileBefore = result.["compileBefore"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    let compile = result.["compile"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    let compileAfter = result.["compileAfter"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    let defineConstants = result.["defineConstants"] :?> TomlArray |> Seq.cast<string> |> Array.ofSeq
                    Some {project = project;refs = refs; compileBefore = compileBefore; compile = compile; compileAfter = compileAfter; defineConstants = defineConstants}
                | _ -> None


            if verbose then
                printfn "Plugins found:"
                plugins |> List.iter (printfn "- '%s'")

            let generators =
                plugins
                |> List.collect findPlugins

            if verbose then
                printfn "Generators found:"
                generators |> List.iter (fun t -> printfn $"- '%s{t.FullName}'")

            let runGenerator (inputFile: string) (genType: Type) =
                let instance = Activator.CreateInstance(genType) :?> IMyriadGenerator

                let configHandler = getConfigHandler verbose config

                if verbose then
                    printfn $"Executing: %s{genType.FullName}..."

                let result =
                    try
                        if instance.ValidInputExtensions |> Seq.contains (Path.GetExtension(inputFile))
                        then
                            let context = GeneratorContext.Create(configKey, configHandler, inputFile, projectContext, additionalParams)
                            Some (instance.Generate(context))
                        else None
                    with
                    | exc ->
                        // emit the module with exception text
                        let info = SynComponentInfo.Create (Ident.CreateLong $"%s{genType.Name}Failure")
                        let pattern =
                            // intentionally generating invalid identifier name to fail the compilation
                            let name = LongIdentWithDots.CreateString "!CompilationError"
                            SynPat.CreateLongIdent(name, [])
                        let letBinding = SynBinding.Let(pattern = pattern, expr = SynExpr.CreateConstString (exc.ToString()))
                        let modulDecl = SynModuleDecl.CreateNestedModule(info, [SynModuleDecl.CreateLet [letBinding]])
                        Some (Output.Ast [SynModuleOrNamespace.CreateNamespace(Ident.CreateLong "", isRecursive = true, decls = [modulDecl])])

                if verbose then printfn $"Result: '%A{result}'"

                result

            if verbose then
                printfn "Execute generators:"
                printfn $"Input Filename:\n:%A{inputFile}"

            let generated =
                generators
                |> List.choose (runGenerator inputFile)

            let formattedCode =
                let cfg = { FormatConfig.FormatConfig.Default with StrictMode = true }
                
                let parseTree =
                    let filename =
                        if inlineGeneration then
                            inputFile
                        else if outputFile.IsSome then
                            outputFile.Value
                        else failwith "Error: No OutputFile was included, and --selfgeneration was not specified."
                    generated
                    |> List.map (fun f ->
                        match f with
                        | Output.Ast ast ->
                            let parseTree = ParsedInput.ImplFile(ParsedImplFileInput.CreateFs(filename, modules = ast))
                            if verbose then    
                                printfn "Generated Ast:------------------------------------"
                                printfn $"%A{parseTree}"
                                printfn "--------------------------------------------------"
                            CodeFormatter.FormatASTAsync(parseTree, "myriad.fsx", [], None, cfg) |> Async.RunSynchronously
                        | Output.Source source -> source )
                    


                parseTree |> String.concat Environment.NewLine
            
            let code =  Generation.getHeaderedCode formattedCode
            if verbose then
                printfn $"Generated Code:\n%A{code}"

            if inlineGeneration then
                let tempFile = Path.GetTempFileName()
                let linesToKeep = Generation.linesToKeep inputFile

                if verbose then printfn $"Inline generation: Writing to temp file: '%A{tempFile}'"
                File.WriteAllLines(tempFile, seq{ yield! linesToKeep; yield! code} )
                if verbose then printfn $"Inline generation: Removing input file: '%A{tempFile}'"
                File.Delete(inputFile)
                if verbose then
                    printfn $"Inline generation: Renaming temp file to input file: '%A{tempFile}' -> '%A{inputFile}'"
                File.Move(tempFile, inputFile)
            else
                match outputFile with
                | Some filename ->
                    if verbose then printfn $"Code generation: Writing output file: '%A{filename}'"
                    File.WriteAllLines(filename, code)
                | None -> failwith "Error: No OutputFile was included, and --inlinegeneration was not specified."

            0 // return an integer exit code

        with
        | :? ArguParseException as ae ->
            printfn $"%s{ae.Message}"
            match ae.ErrorCode with
            | ErrorCode.HelpText -> 0
            | _ -> 2
        | :? ArguParseException as ae when ae.ErrorCode = ErrorCode.HelpText ->
            printfn $"%s{ae.Message}"
            3
        | :? FileNotFoundException as fnf ->
            printfn $"ERROR: inputfile %s{fnf.FileName} doesn not exist\n%s{parser.PrintUsage()}"
            4
