namespace Myriad.Plugins.Example1

open System
open Myriad.Core
open FSharp.Compiler.SyntaxTree
open FsAst

[<MyriadGenerator>]
type Example1Gen() =
    interface IMyriadGenerator with
        member __.Generate(configGetter, _ast) =

            let let42 =
                SynModuleDecl.CreateLet
                    [ { SynBindingRcd.Let with
                            Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "fourtyTwo", [])
                            Expr = SynExpr.CreateConst(SynConst.Int32 42) } ]

            let componentInfo = SynComponentInfoRcd.Create [ Ident.Create "example1" ]
            let nestedModule = SynModuleDecl.CreateNestedModule(componentInfo, [ let42 ])

            let namespace' = "Example"
            let namespaceOrModule =
                { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with Declarations = [ nestedModule ] }

            [namespaceOrModule]