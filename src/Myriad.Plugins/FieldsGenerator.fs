namespace Myriad.Plugins

open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core

module internal Create =
    open FSharp.Compiler.Range

    let createFieldMap (parent: LongIdent) (field: SynField)  =
        let field = field.ToRcd
        let fieldName = match field.Id with None -> failwith "no field name" | Some f -> f

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let varName = "x"
        let pattern =
            let name = LongIdentWithDots.CreateString fieldName.idText
            let arg =
                let named = SynPatRcd.CreateNamed(Ident.Create varName, SynPatRcd.CreateWild)
                SynPatRcd.CreateTyped(named, recordType)
                |> SynPatRcd.CreateParen

            SynPatRcd.CreateLongIdent(name, [arg])

        let expr =
            let ident = LongIdentWithDots.Create [varName; fieldName.idText]
            SynExpr.CreateLongIdent(false, ident, None)

        let valData =
            let argInfo = SynArgInfo.CreateIdString "x"
            let valInfo = SynValInfo.SynValInfo([[argInfo]], SynArgInfo.Empty)
            SynValData.SynValData(None, valInfo, None)

        SynModuleDecl.CreateLet [{SynBindingRcd.Let with
                                    Pattern = pattern
                                    Expr = expr
                                    ValData = valData }]

    let createCreate (parent: LongIdent) (fields: SynField list) =
        let varIdent = LongIdentWithDots.CreateString "create"

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let camelCaseIdent (ident: Ident) =
            Ident.Create(ident.idText.Substring(0, 1).ToLowerInvariant() + ident.idText.Substring(1))

        let pattern =
            let arguments =
                fields
                |> List.map (fun f -> let field = f.ToRcd
                                      let name = SynPatRcd.CreateNamed(camelCaseIdent field.Id.Value, SynPatRcd.CreateWild)
                                      SynPatRcd.CreateTyped(name, field.Type) |> SynPatRcd.CreateParen)

            SynPatRcd.CreateLongIdent(varIdent, arguments)

        let expr =
            let fields =
                fields
                |> List.map (fun f -> let field = f.ToRcd
                                      let fieldIdent = match field.Id with None -> failwith "no field name" | Some f -> f
                                      let name = LongIdentWithDots.Create([fieldIdent.idText])
                                      let ident = SynExpr.CreateIdent(camelCaseIdent fieldIdent)
                                      RecordFieldName(name, true), Some ident, None)

            let newRecord = SynExpr.Record(None, None, fields, range.Zero )
            SynExpr.CreateTyped(newRecord, recordType)

        let returnTypeInfo = SynBindingReturnInfoRcd.Create recordType
        SynModuleDecl.CreateLet [{SynBindingRcd.Let with Pattern = pattern; Expr = expr; ReturnInfo = Some returnTypeInfo }]

    let createMap (recordId: LongIdent) (recordFields: SynField list) : SynModuleDecl =
        let varIdent = LongIdentWithDots.CreateString "map"
        let recordPrimeIdent =  Ident.Create "record'"

        let createFieldMapNameIdent field =
            Ident.Create ("map" + field.Id.Value.idText)

        let pattern =
            let arguments =
                recordFields
                |> List.map (fun f ->let field = f.ToRcd
                                     let fieldType = field.Type
                                     let funType = SynType.Fun(fieldType, fieldType, range0 )
                                     let ident = createFieldMapNameIdent field
                                     let name = SynPatRcd.CreateNamed(ident, SynPatRcd.CreateWild)
                                     SynPatRcd.CreateTyped(name, funType)
                                     |> SynPatRcd.CreateParen)

            let recordParam =
                let name = SynPatRcd.CreateNamed(recordPrimeIdent, SynPatRcd.CreateWild)
                let typ =
                    LongIdentWithDots.Create (recordId |> List.map (fun i -> i.idText))
                    |> SynType.CreateLongIdent

                SynPatRcd.CreateTyped(name, typ)
                |> SynPatRcd.CreateParen

            let allArgs = [yield! arguments; yield recordParam]

            SynPatRcd.CreateLongIdent(varIdent, allArgs)

        let expr =
            let copyInfo =
                let blockSep = (range.Zero, None) : BlockSeparator
                Some (SynExpr.CreateIdent recordPrimeIdent, blockSep)

            let fieldUpdates =
                let mapField (f: SynField) =
                    let f = f.ToRcd
                    let lid = LongIdentWithDots.Create [f.Id.Value.idText]
                    let rfn = RecordFieldName(lid, true)

                    let update =
                        let funcExpr = SynExpr.CreateIdent (createFieldMapNameIdent f)
                        let argExpr =
                            let longIdentWithDots = LongIdentWithDots.Create [recordPrimeIdent.idText; f.Id.Value.idText]
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
            |> SynBindingReturnInfoRcd.Create

        SynModuleDecl.CreateLet
            [ { SynBindingRcd.Let with
                    Pattern = pattern
                    Expr = expr
                    ReturnInfo = Some returnTypeInfo }
            ]

    let createRecordModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) (config: (string * obj) seq) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _range) ->

            let openParent = SynModuleDecl.CreateOpen (LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText)))

            let fieldMaps = recordFields |> List.map (createFieldMap recordId)

            let create = createCreate recordId recordFields

            let map = createMap recordId recordFields

            let declarations = [
                yield openParent
                yield! fieldMaps
                yield create
                yield map ]

            let info = SynComponentInfoRcd.Create recordId
            let mdl = SynModuleDecl.CreateNestedModule(info, declarations)
            let fieldsNamespace =
                config
                |> Seq.tryPick (fun (n,v) -> if n = "namespace" then Some (v :?> string) else None  )
                |> Option.defaultValue "UnknownNamespace"

            {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong fieldsNamespace)
                with
                    IsRecursive = true
                    Declarations = [mdl] }
        | _ -> failwithf "Not a record type"

[<MyriadGenerator("fields")>]
type FieldsGenerator() =

    interface IMyriadGenerator with
        member __.ValidInputExtensions = seq {".fs"}
        member __.Generate(_myriadConfigKey, configGetter: string -> seq<string * obj>, inputFile: string) =
            //_myriadConfigKey is not currently used but could be a failover config section to use when the attribute passes no config section, or used as a root config
            let ast =
                Ast.fromFilename inputFile
                |> Async.RunSynchronously
                |> Array.head
                |> fst

            let namespaceAndrecords =
                Ast.extractRecords ast
                |> List.choose (fun (ns, types) -> 
                    match types |> List.filter (Ast.hasAttribute<Generator.FieldsAttribute>) with
                    | [] -> None
                    | types -> Some (ns, types))

            let modules =
                namespaceAndrecords
                |> List.collect (fun (ns, records) ->
                                    records
                                    |> List.map (fun record -> let config = Generator.getConfigFromAttribute<Generator.FieldsAttribute> configGetter record
                                                               let recordModule = Create.createRecordModule ns record config
                                                               recordModule))

            modules
