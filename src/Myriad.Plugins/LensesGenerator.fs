namespace Myriad.Plugins

open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core
open FSharp.Compiler.Range

module internal CreateLenses =

    let createFieldMap (parent: LongIdent) (field: SynField) =
        let r = range0
        
        let field = field.ToRcd
        let fieldName = match field.Id with None -> failwith "no field name" | Some f -> f

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let name = LongIdentWithDots.CreateString fieldName.idText
            SynPatRcd.CreateLongIdent(name, [])

        let expr =
            let srcVarName = "x"
            let srcIdent = Ident.Create srcVarName

            // x.Property
            let getBody = LongIdentWithDots.Create [srcVarName; fieldName.idText]
            let recordArg = SynSimplePat.Typed(SynSimplePat.Id (srcIdent, None, false, false, false, r), recordType, r)
            // (x : Record)
            let getArgs = SynSimplePats.SimplePats ([recordArg], r)
            // fun (x : Record) -> x.Property
            let get = SynExpr.Lambda (false, false, getArgs, SynExpr.CreateLongIdent(false, getBody, None), r)

            let valueIdent = Ident.Create "value"
            let valuePattern = SynSimplePat.Typed(SynSimplePat.Id (valueIdent, None, false, false, false, r), field.Type, r)
            // (value : PropertyType)
            let valueArgPatterns = SynSimplePats.SimplePats ([valuePattern], r)
            let copySrc = SynExpr.CreateLongIdent(false, LongIdentWithDots.Create [srcVarName], None)
            let recordToUpdateName : RecordFieldName = (LongIdentWithDots.CreateString fieldName.idText, true)
            // { x with Property = value }
            let recordUpdate =
                SynExpr.CreateRecordUpdate (copySrc, [(recordToUpdateName, SynExpr.Ident valueIdent |> Some, None)])

            // (value : PropertyType) -> { x with Property = value }
            let innerLambdaWithValue =
                SynExpr.Lambda (false, true, valueArgPatterns, recordUpdate, r)

            // fun (x : Record) (value : PropertyType) -> { x with Property = value }
            let set =
                SynExpr.Lambda (false, true, getArgs, innerLambdaWithValue, r)

            SynExpr.CreateTuple [
                get
                set
            ]

        let valData =
            let valInfo = SynValInfo.SynValInfo([[]], SynArgInfo.Empty)
            SynValData.SynValData(None, valInfo, None)

        SynModuleDecl.CreateLet [{SynBindingRcd.Let with
                                    Pattern = pattern
                                    Expr = expr
                                    ValData = valData }]

    let private updateLast list updater =
        let folder item state =
            match state with
            | [] -> [updater item]
            | l -> item :: l

        List.foldBack folder list []

    let createRecordModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _range) ->

            let openParent = SynModuleDecl.CreateOpen (LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText)))

            let fieldMaps = recordFields |> List.map (createFieldMap recordId)

            let declarations = [
                yield openParent
                yield! fieldMaps ]

            let ident = updateLast recordId (fun i -> Ident.Create (sprintf "%sLenses" i.idText))

            let info = SynComponentInfoRcd.Create ident
            SynModuleDecl.CreateNestedModule(info, declarations)
        | _ -> failwithf "Not a record type"

[<MyriadGenerator("lenses")>]
type LensesGenerator() =

    interface IMyriadGenerator with
        member __.Generate(namespace', ast: ParsedInput) =
            let namespaceAndRecords = Ast.extractRecords ast
            let modules =
                namespaceAndRecords
                |> List.collect (fun (ns, records) ->
                                    records
                                    |> List.filter (Ast.hasAttribute<Generator.LensesAttribute>)
                                    |> List.map (CreateLenses.createRecordModule ns))

            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = modules }

            namespaceOrModule
