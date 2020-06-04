namespace Myriad.Plugins

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc
open FsAst
open Myriad.Core
open FSharp.Compiler.Range

module internal CreateLenses =
    let r = range0

    let private createLensForRecordField (parent: LongIdent) (wrapperName : Option<string>) (field: SynField) =
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
            | Some name when not (System.String.IsNullOrWhiteSpace(name)) ->
                let wrapperVar = SynExpr.CreateLongIdent (false, LongIdentWithDots (Ident.CreateLong name, []), None)
                SynExpr.App (ExprAtomicFlag.NonAtomic, false, wrapperVar, SynExpr.CreateParen tuple, r)

            | _ -> tuple

        let valData =
            let valInfo = SynValInfo.SynValInfo([[]], SynArgInfo.Empty)
            SynValData.SynValData(None, valInfo, None)

        SynModuleDecl.CreateLet [{SynBindingRcd.Let with
                                    Pattern = pattern
                                    Expr = expr
                                    ValData = valData }]

    let private createLensForDU (parent: LongIdent) (wrapperName : Option<string>) (du : SynUnionCase) =
        let case = du.ToRcd
        let singleCase =
            match case.Type with
            | UnionCaseFields ([singleCase]) -> singleCase
            | UnionCaseFields (_ :: _) -> failwith "It is impossible to create a lens for a DU with several cases"
            | _ -> failwithf "Unsupported type"

        let inputIdent = "x"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let getterName = Ident("getter", range.Zero)
        let pattern =
            SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "_Lens", [])

        let expr =
            let matchCase =
                let indent = LongIdentWithDots.CreateString (case.Id.idText)
                let args = [SynPatRcd.CreateLongIdent (LongIdentWithDots.CreateString "x", [])]
                let p = SynPatRcd.CreateLongIdent(indent, args)

                let rhs = SynExpr.CreateIdent (Ident.Create inputIdent)
                SynMatchClause.Clause(p.FromRcd, None, rhs, range.Zero, DebugPointForTarget.No)

            let matchOn =
                let ident = LongIdentWithDots.CreateString inputIdent
                SynExpr.CreateLongIdent(false, ident, None)

            let mtch = SynExpr.Match(NoDebugPointAtLetBinding, matchOn, [matchCase], range.Zero)

            let valueIdent = Ident.Create "value"
            let valuePattern = SynSimplePat.Typed(SynSimplePat.Id (valueIdent, None, false, false, false, r), singleCase.ToRcd.Type, r)
            let valueArgPatterns = SynSimplePats.SimplePats ([valuePattern], r)

            let duType =
                LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
                |> SynType.CreateLongIdent

            let createCase =
                SynExpr.App (ExprAtomicFlag.NonAtomic, false, SynExpr.Ident case.Id, SynExpr.Ident valueIdent, r)
            let innerLambdaWithValue =
                SynExpr.Lambda (false, true, valueArgPatterns, createCase, r)
            let recordArg = SynSimplePat.Typed(SynSimplePat.Id (Ident.Create "_", None, false, false, false, r), duType, r)
            let getArgs = SynSimplePats.SimplePats ([recordArg], r)

            let set =
                SynExpr.Lambda (false, true, getArgs, innerLambdaWithValue, r)
            let tuple =
                SynExpr.CreateTuple [
                    SynExpr.Ident getterName
                    set
                ]

            let valData = SynValData.SynValData(None, SynValInfo.Empty, None)
            let synPat = SynPat.Named(SynPat.Wild r, Ident.Create "x", false, None, r)
            let synPat = SynPat.Typed(synPat, duType, r)
            let synPat = SynPat.Paren (synPat, r)

            let synPat = SynPat.LongIdent (LongIdentWithDots.CreateString "getter", None, None, SynArgPats.Pats [synPat], None, r)

            let getterSyn = SynBinding.Binding (None, SynBindingKind.NormalBinding, false, false, [], PreXmlDoc.Empty, valData, synPat, None, mtch, r, DebugPointForBinding.NoDebugPointAtDoBinding)
            let lens = SynExpr.LetOrUse (false, false, [getterSyn], tuple, r)
            match wrapperName with
            | Some name when not (System.String.IsNullOrWhiteSpace(name)) ->
                let wrapperVar = SynExpr.CreateLongIdent (false, LongIdentWithDots (Ident.CreateLong name, []), None)
                SynExpr.App (ExprAtomicFlag.NonAtomic, false, wrapperVar, SynExpr.CreateParen lens, r)

            | _ -> lens

        let returnTypeInfo = SynBindingReturnInfoRcd.Create duType
        SynModuleDecl.CreateLet [{SynBindingRcd.Let with
                                      Pattern = pattern
                                      Expr = expr
                                      ReturnInfo = Some returnTypeInfo }]
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

        // Append Lenses to the module name
        let moduleIdent = updateLast recordId (fun i -> Ident.Create (sprintf "%sLenses" i.idText))

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

            let fieldMaps = recordFields |> List.map (createLensForRecordField recordId wrapperName)

            let declarations = [
                yield openParent
                yield! fieldMaps]

            let info = SynComponentInfoRcd.Create moduleIdent
            SynModuleDecl.CreateNestedModule(info, declarations)
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_accessibility, [singleCase], _recordRange), _range) ->
            let openParent = SynModuleDecl.CreateOpen (LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText)))
            let lens = createLensForDU recordId wrapperName singleCase
            let declarations = [
                openParent
                lens
            ]

            let info = SynComponentInfoRcd.Create moduleIdent
            SynModuleDecl.CreateNestedModule(info, declarations)
        | _ -> failwithf "%A is not a record type." recordId

[<MyriadGenerator("lenses")>]
type LensesGenerator() =

    interface IMyriadGenerator with
        member __.Generate(namespace', ast: ParsedInput) =
            let namespaceAndRecords = Ast.extractRecords ast
            let recordsModules =
                namespaceAndRecords
                |> List.collect (
                    fun (ns, records) ->
                    records
                    |> List.map (fun r ->
                        let attr = Ast.getAttribute<Generator.LensesAttribute> r
                        Option.map (fun a -> r, a) attr)
                    |> List.choose id
                    |> List.map (CreateLenses.createRecordModule ns))

            let namespaceAndDUs = Ast.extractDU ast
            let duModules =
                namespaceAndDUs
                |> List.collect (
                    fun (ns, dus) ->
                    dus
                    |> List.map (fun r ->
                        let attr = Ast.getAttribute<Generator.LensesAttribute> r
                        Option.map (fun a -> r, a) attr)
                    |> List.choose id
                    |> List.map (CreateLenses.createRecordModule ns))

            let namespaceOrModule =
                { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = recordsModules @ duModules }

            // todo brinchuk support single-case DU
            namespaceOrModule
