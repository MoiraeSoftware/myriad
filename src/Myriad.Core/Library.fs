namespace Myriad.Core

open System
open Microsoft.FSharp.Compiler.Ast
open FsAst

[<MyriadSdkGenerator("fields")>]
type FieldsGenerator() =

    interface IMyriadGen with
        member __.Generate(namespace', ast: ParsedInput) =
            //check for valid attribute

            let namespaceAndrecords = Ast.extractRecords ast
            let modules =
                namespaceAndrecords
                |> List.collect (fun (ns, records) ->
                                    records
                                    |> List.filter Ast.hasFieldsAttribute
                                    |> List.map (Create.createRecordModule ns))

            let namespace' =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = modules }

            namespace'
