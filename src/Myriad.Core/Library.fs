namespace Myriad.Core

open System
open Microsoft.FSharp.Compiler.Ast
open FsAst

type MyriadGenAttribute(generator: string) =
    inherit Attribute()
    
    member __.Generator = generator

type MyriadSdkGeneratorAttribute(name: string) =
    inherit Attribute()

    member __.Name = name

type IMyriadGen =
    abstract member Generate: namespace': string * ast:ParsedInput -> SynModuleOrNamespaceRcd
    
[<MyriadSdkGenerator("fields")>]
type Example1Gen() =
    interface IMyriadGen with
        member __.Generate(namespace', ast: ParsedInput) =
            
            let records = Ast.extractRecordMeta ast
            let modules = records |> List.map Ast.createRecordModule
            
            let namespace' =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with    
                        IsRecursive = true
                        Declarations = modules }
            
            namespace'
