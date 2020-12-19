namespace Myriad.Plugins.Example1

open System
open System.IO
open Myriad.Core
open FSharp.Compiler.SyntaxTree
open FsAst

[<MyriadGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGenerator with
        member __.ValidInputExtensions = seq {".txt"}
        member __.Generate(configGetter, inputFilename) =

            let let42 =
                SynModuleDecl.CreateLet
                    [ { SynBindingRcd.Let with
                            Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "fourtyTwo", [])
                            Expr = SynExpr.CreateConst(SynConst.Int32 42) } ]


            let allModules = 
                File.ReadAllLines inputFilename
                |> Seq.map (fun moduleName -> 
                                    let componentInfo = SynComponentInfoRcd.Create [ Ident.Create (moduleName) ]
                                    let module' = SynModuleDecl.CreateNestedModule(componentInfo, [ let42 ])
                                    module')
                |> Seq.toList
            

            let namespace' = "Example"
            let namespaceOrModule =
                { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with Declarations = allModules }

            [namespaceOrModule]