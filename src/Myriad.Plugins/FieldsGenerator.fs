namespace Myriad.Plugins

open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core
open Myriad.Core.Ast

module internal Create =
    open FSharp.Compiler.Range

    let createFieldMap (parent: LongIdent) (field: SynField)  =
        let (SynField.Field(_,_,id,_,_,_,_,_)) = field
        let fieldName = match id with None -> failwith "no field name" | Some f -> f

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let varName = "x"
        let pattern =
            let name = LongIdentWithDots.CreateString fieldName.idText
            let arg =
                let named = SynPat.CreateNamed(Ident.Create varName, SynPat.CreateWild)
                SynPat.CreateTyped(named, recordType)
                |> SynPat.CreateParen

            SynPat.CreateLongIdent(name, [arg])

        let expr =
            let ident = LongIdentWithDots.Create [varName; fieldName.idText]
            SynExpr.CreateLongIdent(false, ident, None)

        let valData =
            let argInfo = SynArgInfo.CreateIdString "x"
            let valInfo = SynValInfo.SynValInfo([[argInfo]], SynArgInfo.Empty)
            SynValData.SynValData(None, valInfo, None)

        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr, valData = valData)]

    let createCreate (parent: LongIdent) (fields: SynField list) =
        let varIdent = LongIdentWithDots.CreateString "create"

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let arguments =
                fields
                |> List.map (fun (SynField.Field(_,_,id,typ,_,_,_,_)) ->
                                 let name = SynPat.CreateNamed(Ast.Ident.asCamelCase id.Value, SynPat.CreateWild)
                                 SynPat.CreateTyped(name, typ) |> SynPat.CreateParen)

            SynPat.CreateLongIdent(varIdent, arguments)

        let expr =
            let fields =
                fields
                |> List.map (fun (SynField.Field(_,_,id,_,_,_,_,_)) ->
                                 let fieldIdent = match id with None -> failwith "no field name" | Some f -> f
                                 let name = LongIdentWithDots.Create([fieldIdent.idText])
                                 let ident = SynExpr.CreateIdent(Ast.Ident.asCamelCase fieldIdent)
                                 RecordFieldName(name, true), Some ident, None)

            let newRecord = SynExpr.Record(None, None, fields, range.Zero )
            SynExpr.CreateTyped(newRecord, recordType)

        let returnTypeInfo = SynBindingReturnInfo.Create(recordType)
        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr, returnInfo = returnTypeInfo)]

    let createMap (recordId: LongIdent) (recordFields: SynField list) : SynModuleDecl =
        let varIdent = LongIdentWithDots.CreateString "map"
        let recordPrimeIdent =  Ident.Create "record'"

        let createFieldMapNameIdent (id: Ident option ) =
            Ident.Create ("map" + id.Value.idText)

        let pattern =
            let arguments =
                recordFields
                |> List.map (fun (SynField.Field(_,_,id, fieldType,_,_,_,_)) ->
                                 let funType = SynType.CreateFun(fieldType, fieldType)
                                 let ident = createFieldMapNameIdent id
                                 let name = SynPat.CreateNamed(ident, SynPat.CreateWild)
                                 SynPat.CreateTyped(name, funType)
                                 |> SynPat.CreateParen)

            let recordParam =
                let name = SynPat.CreateNamed(recordPrimeIdent, SynPat.CreateWild)
                let typ =
                    LongIdentWithDots.Create (recordId |> List.map (fun i -> i.idText))
                    |> SynType.CreateLongIdent

                SynPat.CreateTyped(name, typ)
                |> SynPat.CreateParen

            let allArgs = [yield! arguments; yield recordParam]

            SynPat.CreateLongIdent(varIdent, allArgs)

        let expr =
            let copyInfo =
                let blockSep = (range.Zero, None) : BlockSeparator
                Some (SynExpr.CreateIdent recordPrimeIdent, blockSep)

            let fieldUpdates =
                let mapField (SynField.Field(_,_,id,_,_,_,_,_)) =
                    let lid = LongIdentWithDots.Create [id.Value.idText]
                    let rfn = RecordFieldName(lid, true)

                    let update =
                        let funcExpr = SynExpr.CreateIdent (createFieldMapNameIdent id)
                        let argExpr =
                            let longIdentWithDots = LongIdentWithDots.Create [recordPrimeIdent.idText; id.Value.idText]
                            SynExpr.CreateLongIdent(false, longIdentWithDots, None)
                        SynExpr.CreateApp(funcExpr, argExpr)

                    rfn, Some update, (None : Option<BlockSeparator>)

                let arguments =
                    recordFields
                    |> List.map mapField

                arguments

            SynExpr.Record(None, copyInfo, fieldUpdates, range.Zero )

        let returnTypeInfo =
            LongIdentWithDots.Create (recordId |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent
            |> SynBindingReturnInfo.Create

        SynModuleDecl.CreateLet
            [ SynBinding.Let(pattern = pattern, expr = expr, returnInfo = returnTypeInfo) ]

    let createRecordModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) (config: (string * obj) seq) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _)) = synComponentInfo
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _) ->

            let ident = LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText))
            let openTarget = SynOpenDeclTarget.ModuleOrNamespace(ident.Lid, range0)
            let openParent = SynModuleDecl.CreateOpen openTarget

            let fieldMaps = recordFields |> List.map (createFieldMap recordId)

            let create = createCreate recordId recordFields

            let map = createMap recordId recordFields

            let declarations = [
                yield openParent
                yield! fieldMaps
                yield create
                yield map ]

            let info = SynComponentInfo.Create recordId
            let mdl = SynModuleDecl.CreateNestedModule(info, declarations)
            let fieldsNamespace =
                config
                |> Seq.tryPick (fun (n,v) -> if n = "namespace" then Some (v :?> string) else None  )
                |> Option.defaultValue "UnknownNamespace"

            SynModuleOrNamespace.CreateNamespace(Ident.CreateLong fieldsNamespace, isRecursive = true, decls = [mdl])
        | _ -> failwithf "Not a record type"

[<MyriadGenerator("fields")>]
type FieldsGenerator() =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq {".fs"}
        member _.Generate(context: GeneratorContext) =
            //_myriadConfigKey is not currently used but could be a failover config section to use when the attribute passes no config section, or used as a root config
            let ast, _ =
                Ast.fromFilename context.InputFilename
                |> Async.RunSynchronously
                |> Array.head

            let namespaceAndrecords =
                Ast.extractRecords ast
                |> List.choose (fun (ns, types) ->
                    match types |> List.filter Ast.hasAttribute<Generator.FieldsAttribute> with
                    | [] -> None
                    | types -> Some (ns, types))

            let modules =
                namespaceAndrecords
                |> List.collect (fun (ns, records) ->
                                    records
                                    |> List.map (fun record -> let config = Generator.getConfigFromAttribute<Generator.FieldsAttribute> context.ConfigGetter record
                                                               let recordModule = Create.createRecordModule ns record config
                                                               recordModule))

            modules
