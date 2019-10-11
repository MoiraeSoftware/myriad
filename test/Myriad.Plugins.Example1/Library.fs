namespace Myriad.Plugins.Example1

open Myriad.Core
open Microsoft.FSharp.Compiler.Ast
open FsAst

[<MyriadSdkGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGen with
        member __.Generate(namespace', _ast) =
            let ident = LongIdentWithDots.CreateString "test"
            let lid = ident.Lid
            let modu = SynModuleOrNamespaceRcd.CreateModule(lid)
            
            let let42 =
                SynModuleDecl.CreateLet
                    [{SynBindingRcd.Let with
                        Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "fourtyTwo", [])
                        Expr = SynExpr.CreateConst(SynConst.Int32 42)
                        (*ValData = valData*) }]
            
            let final = modu.AddDeclaration let42
            final
