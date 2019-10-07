namespace Myriad.Plugins.Example1

open Myriad.Core
open Microsoft.FSharp.Compiler.Ast
open FsAst

[<MyriadSdkGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGen with
        member __.Generate(namespace', ast) =
            let ident = LongIdentWithDots.CreateString "test"
            let lid = ident.Lid
            SynModuleOrNamespaceRcd.CreateModule(lid)
