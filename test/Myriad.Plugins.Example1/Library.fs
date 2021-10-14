namespace Myriad.Plugins.Example1

open System
open System.IO
open Myriad.Core
open Myriad.Core.Ast
open FSharp.Compiler.SyntaxTree
open FsAst

[<MyriadGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq {".txt"}
        member _.Generate(context: GeneratorContext) =

            let example1Namespace =
                context.ConfigKey
                |> Option.map context.ConfigGetter
                |> Option.bind (Seq.tryPick (fun (n,v) -> if n = "namespace" then Some (v :?> string) else None ))
                |> Option.defaultValue "UnknownNamespace"

            let let42 =
                SynModuleDecl.CreateLet
                    [ SynBinding.Let(pattern = SynPat.CreateLongIdent(LongIdentWithDots.CreateString "fourtyTwo", []), expr = SynExpr.CreateConst(SynConst.Int32 42)) ]

            let allModules =
                File.ReadAllLines context.InputFilename
                |> Seq.map (fun moduleName ->
                                    let componentInfo = SynComponentInfo.Create [ Ident.Create moduleName ]
                                    let module' = SynModuleDecl.CreateNestedModule(componentInfo, [ let42 ])
                                    module')
                |> Seq.toList

            [SynModuleOrNamespace.CreateNamespace(Ident.CreateLong example1Namespace, decls = allModules)]
