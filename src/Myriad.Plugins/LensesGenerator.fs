namespace Myriad.Plugins

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Xml
open Myriad.Core
open Myriad.Core.Ast
open FSharp.Compiler.Text.Range
open FSharp.Compiler.SyntaxTrivia

module internal CreateLenses =
    let private wrap (wrapperName : Option<string>) lens =
        match wrapperName with
        | Some name when not (String.IsNullOrWhiteSpace(name)) ->
            let wrapperVar = SynExpr.CreateLongIdent (false, SynLongIdent.CreateString name, None)
            SynExpr.App (ExprAtomicFlag.NonAtomic, false, wrapperVar, SynExpr.CreateParen lens, range0)
        | _ -> lens

    let private createLensForRecordField (parent: LongIdent) (wrapperName : Option<string>) (aetherStyle: bool) (field: SynField) =
        let (SynField.SynField(_,_,id,fieldType,_,_,_,_)) = field
        let fieldName = match id with None -> failwith "no field name" | Some f -> f

        let recordType =
            SynLongIdent.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent
                    
        let letPat = SynPat.CreateNamed fieldName
        let lambdaGetBody = SynExpr.CreateLongIdent(SynLongIdent.Create ["x"; fieldName.idText])
        let lambdaGetPats = [SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed(Ident.Create "x"), recordType))]
        
        let lambdaSetBody =
            let innerPats =
                if aetherStyle then
                    [SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed(Ident.Create "value"), fieldType))
                     SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed(Ident.Create "x"), recordType))]
                else
                    [SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed(Ident.Create "x"), recordType))
                     SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed(Ident.Create "value"), fieldType)) ]
                    
            let innerBody =
                let copySrc = SynExpr.CreateLongIdent(false, SynLongIdent.Create ["x"], None)
                let recordUpdateName :RecordFieldName = (SynLongIdent.CreateString fieldName.idText, true)
                let fieldList = [(recordUpdateName, Some(SynExpr.Ident (Ident.Create "value")))]
                SynExpr.CreateRecordUpdate (copySrc, fieldList)
                
            SynExpr.CreateLambda(pats = innerPats, body = innerBody)
        let lambdaSetPats = []
                                              
        let letBody =
            SynExpr.CreateTuple[
                SynExpr.CreateParen(SynExpr.CreateLambda(pats = lambdaGetPats, body = lambdaGetBody))
                SynExpr.CreateParen(SynExpr.CreateLambda(pats = lambdaSetPats, body = lambdaSetBody))] |> wrap wrapperName
            
        SynModuleDecl.CreateLet [SynBinding.Let(pattern = letPat, expr = letBody)]

    let private createLensForDU (requiresQualifiedAccess : bool) (parent: LongIdent) (wrapperName : Option<string>) (du : SynUnionCase) =
        let (SynUnionCase.SynUnionCase(_,(SynIdent(id, _)),duType,_,_,_,_)) = du
        let (SynField.SynField(_,_,_,fieldType,_,_,_,_)) =
            match duType with
            | SynUnionCaseKind.Fields [singleCase] -> singleCase
            | SynUnionCaseKind.Fields (_ :: _) -> failwith "It is not possible to create a lens for a DU with several cases"
            | _ -> failwithf "Unsupported type"

        let duType =
            SynLongIdent.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let getterName = Ident("getter", range0)
        let pattern =
            SynPat.CreateLongIdent(SynLongIdent.CreateString "Lens'", [])

        let matchCaseIdentParts =
            if requiresQualifiedAccess then
                (parent |> List.map (fun i -> i.idText)) @ [id.idText]
            else
                [id.idText]

        // The name of the DU case, optionally preceded by the name of the DU itself, if fully qualified access is required
        let fullCaseName = SynLongIdent.Create matchCaseIdentParts

        let lensExpression =
            let matchCase =
                let caseVariableName = "x"
                let args = [SynPat.CreateLongIdent (SynLongIdent.CreateString caseVariableName, [])]
                let matchCaseIdent = SynPat.CreateLongIdent(fullCaseName, args)

                let rhs = SynExpr.CreateIdent (Ident.Create caseVariableName)
                SynMatchClause.Create(matchCaseIdent, None, rhs)

            let getterArgName = "x"
            let matchOn =
                let ident = SynLongIdent.CreateString getterArgName
                SynExpr.CreateLongIdent(false, ident, None)

            let matchExpression = SynExpr.CreateMatch(matchOn, [matchCase])

            let setter =
                let valueIdent = Ident.Create "value"

                let valueArgPatterns = [SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed valueIdent, fieldType))]

                let duType =
                    SynLongIdent.Create (parent |> List.map (fun i -> i.idText))
                    |> SynType.CreateLongIdent

                let createCase = SynExpr.App (ExprAtomicFlag.NonAtomic, false, SynExpr.LongIdent (false, fullCaseName, None, range0), SynExpr.Ident valueIdent, range0)
                
                let innerLambdaWithValue = SynExpr.CreateLambda([], createCase) //inner does not have pats as they are pushed in via the outer lambda

                let getArgs = [SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed (Ident.Create "_"), duType))
                               SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed valueIdent, fieldType))] //inner lambdas pat ∆

                SynExpr.CreateLambda(pats = getArgs, body = innerLambdaWithValue)

            let tuple = SynExpr.CreateTuple [ SynExpr.Ident getterName; setter ]

            let getterLet =
                let valData = SynValData.SynValData(None, SynValInfo.Empty, None)
                let synPat = SynPat.CreateParen(SynPat.CreateTyped(SynPat.CreateNamed(Ident.Create "x", false), duType))

                let synPat = SynPat.LongIdent (SynLongIdent.CreateString "getter", None, None, SynArgPats.Pats [synPat], None, range0)

                let trivia = {EqualsRange = Some range0; LetKeyword = Some range0}
                SynBinding.SynBinding (None, SynBindingKind.Normal, false, false, [], PreXmlDoc.Empty, valData, synPat, None, matchExpression, range0, DebugPointAtBinding.NoneAtDo, trivia)

            let lens = SynExpr.LetOrUse (false, false, [getterLet], tuple, range0, { InKeyword = None })

            lens |> wrap wrapperName

        SynModuleDecl.CreateLet [ SynBinding.Let(pattern = pattern, expr = lensExpression) ]
    let private updateLastItem list updater =
        let folder item state =
            match state with
            | [] -> [updater item]
            | l -> item :: l

        List.foldBack folder list []

    let private (|LongIdentLid|) (ident : SynLongIdent) =
        ident.LongIdent

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

        let ident = SynLongIdent.Create (namespaceId |> List.map (fun ident -> ident.idText))
        let openTarget = SynOpenDeclTarget.ModuleOrNamespace(ident, range0)
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
