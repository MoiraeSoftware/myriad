namespace Myriad.Plugins

open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core
open FSharp.Compiler.Range

module internal CreateLenses =

    let private createLens (parent: LongIdent) (wrapperName : Option<string>) (field: SynField) =
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

            let tuple =
                SynExpr.CreateTuple [
                    get
                    set
                ]
            match wrapperName with
            | Some name ->
                let wrapperVar = SynExpr.CreateLongIdent (false, LongIdentWithDots (Ident.CreateLong name, []), None)
                SynExpr.App (ExprAtomicFlag.NonAtomic, false, wrapperVar, SynExpr.CreateParen tuple, r)

            | None -> tuple

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

    let private (|IdentText|) (ident : Ident) =
        ident.idText

    let private (|LongIdentLid|) (ident : LongIdentWithDots) =
        ident.Lid

    let private (|SynTypeAppTypeName|_|) (expr : SynType) =
        match expr with
        | SynType.App (name, _, _, _, _, _, _) -> Some name
        | _ -> None

    let createRecordModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn, attr: SynAttribute) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo

        let wrapperName =
            match attr.ArgExpr with
            | SynExpr.Const (SynConst.Unit, _) -> None
            | SynExpr.Paren(SynExpr.Const ((SynConst.String(s, _)), _), _leftParenRange, _rightParenRange, _range) -> Some s
            | SynExpr.Paren(SynExpr.TypeApp (SynExpr.Ident ident, _, [SynTypeAppTypeName(SynType.LongIdent (LongIdentLid (wrapperIdent :: _)))], _, _, _, _), _, _, _)
                when ident.idText = "typedefof" || ident.idText = "typeof" ->
                Some wrapperIdent.idText
            | _ -> failwithf "Unsupported syntax of specifying the wrapper name for type %A." recordId

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _range) ->
            let openParent = SynModuleDecl.CreateOpen (LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText)))

            let fieldMaps = recordFields |> List.map (createLens recordId wrapperName)

            let declarations = [
                yield openParent
                yield! fieldMaps]

            // Append Lenses to the module name
            let ident = updateLast recordId (fun i -> Ident.Create (sprintf "%sLenses" i.idText))

            let info = SynComponentInfoRcd.Create ident
            SynModuleDecl.CreateNestedModule(info, declarations)
        | _ -> failwithf "%A is not a record type." recordId

[<MyriadGenerator("lenses")>]
type LensesGenerator() =

    interface IMyriadGenerator with
        member __.Generate(namespace', ast: ParsedInput) =
            let namespaceAndRecords = Ast.extractRecords ast
            let modules =
                namespaceAndRecords
                |> List.collect (
                    fun (ns, records) ->
                    records
                    |> List.map (fun r -> Ast.getAttribute<Generator.LensesAttribute> r |> Option.map (fun a -> r, a))
                    |> List.choose id
                    |> List.map (CreateLenses.createRecordModule ns))

            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = modules }

            // todo brinchuk support single-case DU
            namespaceOrModule
