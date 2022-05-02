namespace Myriad.Plugins

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Xml
open Myriad.Core
open Myriad.Core.Ast
open FSharp.Compiler.Text.Range
open FSharp.Compiler.SyntaxTrivia
open FsAst

module internal CreateLenses =
    let r = range0
    let private wrap lens (wrapperName : Option<string>) =
        match wrapperName with
        | Some name when not (String.IsNullOrWhiteSpace(name)) ->
            let wrapperVar = SynExpr.CreateLongIdent (false, LongIdentWithDots (Ident.CreateLong name, []), None)
            SynExpr.App (ExprAtomicFlag.NonAtomic, false, wrapperVar, SynExpr.CreateParen lens, r)
        | _ -> lens

    let private createLensForRecordField (parent: LongIdent) (wrapperName : Option<string>) (aetherStyle: bool) (field: SynField) =
        let (SynField.SynField(_,_,id,fieldType,_,_,_,_)) = field
        let fieldName = match id with None -> failwith "no field name" | Some f -> f

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let name = LongIdentWithDots.CreateString fieldName.idText
            SynPat.CreateLongIdent(name, [])      
        
        let expr =
            let srcVarName = "x"
            let srcIdent = Ident.Create srcVarName

            // x.Property
            let getBody = LongIdentWithDots.Create [srcVarName; fieldName.idText]
            //let recordArg = SynSimplePat.Typed(SynSimplePat.Id (srcIdent, None, false, false, false, r), recordType, r)
            
            // (x : Record)
            let getPats = [SynPat.CreateTyped(SynPat.CreateNamed(srcIdent), recordType)]
            
            // fun (x : Record) -> x.Property
            let get = SynExpr.CreateLambda getPats (SynExpr.CreateLongIdent(false, getBody, None))

            let valueIdent = Ident.Create "value"
            let valuePattern = [SynPat.CreateTyped(SynPat.CreateNamed valueIdent, fieldType)]
                //SynSimplePat.Typed(SynSimplePat.Id (valueIdent, None, false, false, false, r), fieldType, r)
            
            // (value : PropertyType)
            //let valueArgPatterns = SynSimplePats.SimplePats ([valuePattern], r)
            let copySrc = SynExpr.CreateLongIdent(false, LongIdentWithDots.Create [srcVarName], None)
            let recordToUpdateName : RecordFieldName = (LongIdentWithDots.CreateString fieldName.idText, true)
            
            // { x with Property = value }
            let recordUpdate = SynExpr.CreateRecordUpdate (copySrc, [(recordToUpdateName, SynExpr.Ident valueIdent |> Some)])

            // (value : PropertyType) -> { x with Property = value }
            let innerLambdaWithValue valueArgs =
                SynExpr.CreateLambda valueArgs recordUpdate

            let set =
                if aetherStyle then
                    // fun (value : PropertyType) -> (x : Record) -> { x with Property = value }
                    SynExpr.CreateLambda valuePattern (innerLambdaWithValue getPats)
                    //SynExpr.Lambda (false, true, valueArgPattern, innerLambdaWithValue getPats, None, r, SynExprLambdaTrivia.Zero)
                else
                    // fun (x : Record) (value : PropertyType) -> { x with Property = value }
                    SynExpr.CreateLambda getPats (innerLambdaWithValue valuePattern)
                    //SynExpr.Lambda (false, true, getPats, innerLambdaWithValue valueArgPattern, None, r, SynExprLambdaTrivia.Zero)

            let tuple = SynExpr.CreateTuple [ SynExpr.CreateParen get; SynExpr.CreateParen set ]

            wrap tuple wrapperName

        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr)]

    let private createLensForDU (requiresQualifiedAccess : bool) (parent: LongIdent) (wrapperName : Option<string>) (du : SynUnionCase) =
        let (SynUnionCase.SynUnionCase(_,id,duType,_,_,_,_)) = du
        let (SynField.SynField(_,_,_,fieldType,_,_,_,_)) =
            match duType with
            | SynUnionCaseKind.Fields [singleCase] -> singleCase
            | SynUnionCaseKind.Fields (_ :: _) -> failwith "It is impossible to create a lens for a DU with several cases"
            | _ -> failwithf "Unsupported type"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let getterName = Ident("getter", range0)
        let pattern =
            SynPat.CreateLongIdent(LongIdentWithDots.CreateString "Lens'", [])

        let matchCaseIdentParts =
            if requiresQualifiedAccess then
                (parent |> List.map (fun i -> i.idText)) @ [id.idText]
            else
                [id.idText]

        // The name of the DU case, optionally preceded by the name of the DU itself, if
        // fully qualified access is required
        let fullCaseName = LongIdentWithDots.Create matchCaseIdentParts

        let lensExpression =
            let matchCase =
                let caseVariableName = "x"
                let args = [SynPat.CreateLongIdent (LongIdentWithDots.CreateString caseVariableName, [])]
                let matchCaseIdent = SynPat.CreateLongIdent(fullCaseName, args)

                let rhs = SynExpr.CreateIdent (Ident.Create caseVariableName)
                SynMatchClause.Create(matchCaseIdent, None, rhs)

            let getterArgName = "x"
            let matchOn =
                let ident = LongIdentWithDots.CreateString getterArgName
                SynExpr.CreateLongIdent(false, ident, None)

            let matchExpression = SynExpr.CreateMatch(matchOn, [matchCase])

            let setter =
                let valueIdent = Ident.Create "value"
                //let valuePattern = SynSimplePat.Typed(SynSimplePat.Id (valueIdent, None, false, false, false, r), fieldType, r)
                let valueArgPatterns =
                    [SynPat.CreateTyped(SynPat.CreateNamed valueIdent, fieldType)]
                    //SynSimplePats.SimplePats ([valuePattern], r)

                let duType =
                    LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
                    |> SynType.CreateLongIdent

                let createCase = SynExpr.App (ExprAtomicFlag.NonAtomic, false, SynExpr.LongIdent (false, fullCaseName, None, r), SynExpr.Ident valueIdent, r)
                
                let innerLambdaWithValue =
                    //SynExpr.Lambda (false, true, valueArgPatterns, createCase, None, r, SynExprLambdaTrivia.Zero)
                    SynExpr.CreateLambda valueArgPatterns createCase
                //let recordArg = SynSimplePat.Typed(SynSimplePat.Id (Ident.Create "_", None, false, false, false, r), duType, r)
                let getArgs =
                    [SynPat.CreateTyped(SynPat.CreateNamed (Ident.Create "_"), duType)]
                    //SynSimplePats.SimplePats ([recordArg], r)

                //SynExpr.Lambda (false, true, getArgs, innerLambdaWithValue, None, r, SynExprLambdaTrivia.Zero)
                SynExpr.CreateLambda getArgs innerLambdaWithValue

            let tuple = SynExpr.CreateTuple [ SynExpr.Ident getterName; setter ]

            let getterLet =
                let valData = SynValData.SynValData(None, SynValInfo.Empty, None)
                let synPat = SynPat.Named(Ident.Create "x", false, None, r)
                let synPat = SynPat.Typed(synPat, duType, r)
                let synPat = SynPat.Paren (synPat, r)

                let synPat = SynPat.LongIdent (LongIdentWithDots.CreateString "getter", None, None, None, SynArgPats.Pats [synPat], None, r)

                SynBinding.SynBinding (None, SynBindingKind.Normal, false, false, [], PreXmlDoc.Empty, valData, synPat, None, matchExpression, r, DebugPointAtBinding.NoneAtDo, SynBindingTrivia.Zero)

            let lens = SynExpr.LetOrUse (false, false, [getterLet], tuple, r, { InKeyword = None })

            wrap lens wrapperName

        SynModuleDecl.CreateLet [ SynBinding.Let(pattern = pattern, expr = lensExpression) ]
    let private updateLastItem list updater =
        let folder item state =
            match state with
            | [] -> [updater item]
            | l -> item :: l

        List.foldBack folder list []

    let private (|LongIdentLid|) (ident : LongIdentWithDots) =
        ident.Lid

    let private (|SynTypeAppTypeName|_|) (expr : SynType) =
        match expr with
        | SynType.App (name, _, _, _, _, _, _) -> Some name
        | _ -> None

    let createLensModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) (attr: SynAttribute) (usePipedSetter: bool) =
        let (SynTypeDefn(synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _range, _trivia)) = typeDefn
        let (SynComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo

        // Append "Lenses" to the module name
        let moduleIdent = updateLastItem recordId (fun i -> Ident.Create (sprintf $"%s{i.idText}Lenses"))

        let wrapperName =
            match attr.ArgExpr with
            | SynExpr.Const _
            | SynExpr.Paren(SynExpr.Const _,_,_,_) -> None
            | SynExpr.Paren(SynExpr.Tuple(_,[_thisIsTheConfig; SynExpr.Const(SynConst.String(s,_synStringKind,_), _)],_,_),_,_,_) -> Some s
            | SynExpr.Paren(SynExpr.Tuple(_,[_thisIsTheConfig
                                             SynExpr.TypeApp (SynExpr.Ident ident, _, [SynTypeAppTypeName(SynType.LongIdent longIdent)], _, _, _, _)],_,_),_,_,_)
                                             when ident.idText = "typedefof" || ident.idText = "typeof" ->
                                             Some longIdent.AsString
            | expr-> failwithf $"Unsupported syntax of specifying the wrapper name for type %A{recordId}.\nExpr: %A{expr}"

        let ident = LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText))
        let openTarget = SynOpenDeclTarget.ModuleOrNamespace(ident.Lid, r)
        let openParent = SynModuleDecl.CreateOpen openTarget
        let moduleInfo = SynComponentInfo.Create moduleIdent

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_accessibility, recordFields, _recordRange), _range) ->
            let fieldLenses = recordFields |> List.map (createLensForRecordField recordId wrapperName usePipedSetter)
            let declarations = [yield openParent; yield! fieldLenses ]
            SynModuleDecl.CreateNestedModule(moduleInfo, declarations)

        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_accessibility, [singleCase], _recordRange), _range) ->
            let requiresQualifiedAccess = Ast.getAttribute<RequireQualifiedAccessAttribute> typeDefn |> Option.isSome
            let lens = createLensForDU requiresQualifiedAccess recordId wrapperName singleCase
            let declarations = [ openParent; lens ]
            SynModuleDecl.CreateNestedModule(moduleInfo, declarations)

        | _ -> failwithf $"%A{recordId} is not a record type."

[<MyriadGenerator("lenses")>]
type LensesGenerator() =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq {".fs"}
        member _.Generate(context: GeneratorContext) =
            //context.ConfigKey is not currently used but could be a failover config section to use when the attribute passes no config section, or used as a root config
            let ast, _ =
                Ast.fromFilename context.InputFilename
                |> Async.RunSynchronously
                |> Array.head

            let namespaceAndRecords = Ast.extractRecords ast
            let recordsModules =
                namespaceAndRecords
                |> List.collect (
                    fun (ns, records) ->
                    records
                    |> List.choose (fun r ->
                        let attr = Ast.getAttribute<Generator.LensesAttribute> r
                        Option.map (fun a -> r, a) attr)
                    |> List.map (fun (record, attrib) -> let config = Generator.getConfigFromAttribute<Generator.LensesAttribute> context.ConfigGetter record
                                                         let recordsNamespace =
                                                              config
                                                              |> Seq.tryPick (fun (n, v) -> if n = "namespace" then Some (v :?> string) else None  )
                                                              |> Option.defaultValue "UnknownNamespace"
                                                         let usePipedSetter = 
                                                             config
                                                             |> Seq.tryPick (fun (n, v) -> if n = "pipedsetter" then Some (v :?> bool) else None  )
                                                             |> Option.defaultValue false
                                                         let synModule = CreateLenses.createLensModule ns record attrib usePipedSetter
                                                         SynModuleOrNamespace.CreateNamespace(Ident.CreateLong recordsNamespace, isRecursive =true, decls = [synModule])))

            let namespaceAndDUs = Ast.extractDU ast
            let duModules =
                namespaceAndDUs
                |> List.collect (
                    fun (ns, dus) ->
                    dus
                    |> List.choose (fun du ->
                        let attr = Ast.getAttribute<Generator.LensesAttribute> du
                        Option.map (fun a -> du, a) attr)
                    |> List.map (fun (du, attrib) -> let config = Generator.getConfigFromAttribute<Generator.LensesAttribute> context.ConfigGetter du
                                                     let dusNamespace =
                                                         config
                                                         |> Seq.tryPick (fun (n, v) -> if n = "namespace" then Some (v :?> string) else None  )
                                                         |> Option.defaultValue "UnknownNamespace"
                                                     let usePipedSetter = 
                                                         config
                                                         |> Seq.tryPick (fun (n, v) -> if n = "pipedsetter" then Some (v :?> bool) else None  )
                                                         |> Option.defaultValue false
                                                     let synModule = CreateLenses.createLensModule ns du attrib usePipedSetter
                                                     SynModuleOrNamespace.CreateNamespace(Ident.CreateLong dusNamespace, isRecursive = true, decls = [synModule])))

            Output.Ast [yield! recordsModules; yield! duModules]
