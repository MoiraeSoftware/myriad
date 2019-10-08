namespace Myriad.Core

open System
open Microsoft.FSharp.Compiler.Ast
open FsAst
    
[<MyriadSdkGenerator("fields")>]
type FieldsGenerator() =
        
    interface IMyriadGen with
        member __.Generate(namespace', ast: ParsedInput) =
            //check for valid attribute

            
            let records = Ast.extractRecordMeta ast
            let modules = records |> List.map Ast.createRecordModule
            
            let namespace' =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with    
                        IsRecursive = true
                        Declarations = modules }
            
            namespace'
