namespace Myriad.Plugins

open FSharp.Compiler.Syntax
open Myriad.Core
open Myriad.Core.Ast

module internal CreateDUModule =
    open FSharp.Compiler.Text.Range

    let createToString (requiresQualifiedAccess: bool) (parent: LongIdent) (cases: SynUnionCase list) =
        let varIdent = LongIdentWithDots.CreateString "toString"
        let inputIdent = "x"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let ident = Ident(inputIdent, range0)
            let name = SynPat.CreateNamed(ident)
            let args = SynPat.CreateTyped(name, duType) |> SynPat.CreateParen
            SynPat.CreateLongIdent(varIdent, [args])

        let expr =
            let matches =
                cases
                |> List.map (fun (SynUnionCase.SynUnionCase(_,id,_,_,_,_,_) as unionCase) ->
                    let matchCaseIdentParts =
                        if requiresQualifiedAccess then
                            (parent |> List.map (fun i -> i.idText)) @ [id.idText]
                        else
                            [id.idText]
                    let indent = LongIdentWithDots.Create matchCaseIdentParts
                    let args = if unionCase.HasFields then [SynPat.CreateWild] else []                       
                        
                    let p = SynPat.CreateLongIdent(indent, args)
                    let rhs =
                       SynExpr.CreateConst(SynConst.CreateString id.idText)
                    SynMatchClause.Create(p, None, rhs)
                )
            let matchOn =
                let ident = LongIdentWithDots.CreateString inputIdent
                SynExpr.CreateLongIdent(false, ident, None)

            SynExpr.CreateMatch(matchOn, matches)

        let returnTypeInfo = SynBindingReturnInfo.Create duType
        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr, returnInfo = returnTypeInfo)]

    let createFromString (requiresQualifiedAccess: bool) (parent: LongIdent) (cases: SynUnionCase list) =
        let varIdent = LongIdentWithDots.CreateString "fromString"
        let inputIdent = "x"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let inputType =
            LongIdentWithDots.CreateString "string"
            |> SynType.CreateLongIdent

        let pattern =
            let ident = Ident(inputIdent, range0)
            let name = SynPat.CreateNamed(ident)
            let args = SynPat.CreateTyped(name, inputType) |> SynPat.CreateParen
            SynPat.CreateLongIdent(varIdent, [args])

        let expr =
            let matches =
                cases
                //Only provide `fromString` for cases with no fields
                |> List.filter (fun c -> not c.HasFields)
                |> List.map (fun (SynUnionCase.SynUnionCase(_,id,_,_,_,_,_)) ->
                    let con =  SynConst.CreateString id.idText
                    let pat = SynPat.CreateConst(con)
                    let rhs =
                        let f = SynExpr.Ident (Ident("Some", range0))
                        let matchCaseIdentParts =
                            if requiresQualifiedAccess then
                                (parent |> List.map (fun i -> i.idText)) @ [id.idText]
                            else
                                [id.idText]
                        let fullCaseName = LongIdentWithDots.Create matchCaseIdentParts
                        let x = SynExpr.CreateLongIdent fullCaseName
                        SynExpr.App(ExprAtomicFlag.NonAtomic, false, f, x, range0)
                    SynMatchClause.Create(pat, None, rhs)
                )
            let wildCase =
                let rhs = SynExpr.Ident (Ident("None", range0))
                SynMatchClause.Create(SynPat.CreateWild, None, rhs)

            let matchOn =
                let ident = LongIdentWithDots.CreateString inputIdent
                SynExpr.CreateLongIdent(false, ident, None)

            SynExpr.Match(range0, DebugPointAtBinding.NoneAtLet, matchOn, range0, [yield! matches; wildCase], range0)

        let returnTypeInfo = SynBindingReturnInfo.Create duType
        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr, returnInfo = returnTypeInfo)]

    let createToTag (requiresQualifiedAccess: bool) (parent: LongIdent) (cases: SynUnionCase list) =
        let varIdent = LongIdentWithDots.CreateString "toTag"
        let inputIdent = "x"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let ident = Ident(inputIdent, range0)
            let name = SynPat.CreateNamed(ident)
            let args = SynPat.CreateTyped(name, duType) |> SynPat.CreateParen
            SynPat.CreateLongIdent(varIdent, [args])

        let expr =
            let matches =
                cases
                |> List.mapi (fun i case ->
                    let (SynUnionCase.SynUnionCase(_,id,_,_,_,_,_)) = case
                    let matchCaseIdentParts =
                        if requiresQualifiedAccess then
                            (parent |> List.map (fun i -> i.idText)) @ [id.idText]
                        else
                            [id.idText]
                    let indent = LongIdentWithDots.Create matchCaseIdentParts
                    let args = if case.HasFields then [SynPat.CreateWild] else []
                    let p = SynPat.CreateLongIdent(indent, args)
                    let rhs = SynExpr.Const(SynConst.Int32 i, range0)
                    SynMatchClause.Create(p, None, rhs)
                )
            let matchOn =
                let ident = LongIdentWithDots.CreateString inputIdent
                SynExpr.CreateLongIdent(false, ident, None)

            SynExpr.Match(range0, DebugPointAtBinding.NoneAtLet , matchOn, range0, matches, range0)

        let returnTypeInfo = SynBindingReturnInfo.Create duType
        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr, returnInfo = returnTypeInfo)]

    let createIsCase (requiresQualifiedAccess: bool) (parent: LongIdent) (cases: SynUnionCase list) =
        [ for case in cases do
            let (SynUnionCase.SynUnionCase(_,id,_,_,_,_,_)) = case
            let varIdent = LongIdentWithDots.CreateString $"is%s{id.idText}"
            let inputIdent = "x"

            let duType =
                LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
                |> SynType.CreateLongIdent

            let pattern =
                let ident = Ident(inputIdent, range0)
                let name = SynPat.CreateNamed(ident)
                let args = SynPat.CreateTyped(name, duType) |> SynPat.CreateParen
                SynPat.CreateLongIdent(varIdent, [args])

            let expr =
                let matchCase =
                    let matchCaseIdentParts =
                        if requiresQualifiedAccess then
                            (parent |> List.map (fun i -> i.idText)) @ [id.idText]
                        else
                            [id.idText]
                    let indent = LongIdentWithDots.Create matchCaseIdentParts
                    let args = if case.HasFields then [SynPat.CreateWild] else []
                    let p = SynPat.CreateLongIdent(indent, args)

                    let rhs = SynExpr.Const (SynConst.Bool true, range0)
                    SynMatchClause.Create(p, None, rhs)

                let wildCase =
                    let rhs = SynExpr.CreateConst (SynConst.Bool false)
                    SynMatchClause.Create(SynPat.CreateWild, None, rhs)

                let matchOn =
                    let ident = LongIdentWithDots.CreateString inputIdent
                    SynExpr.CreateLongIdent(false, ident, None)

                SynExpr.Match(range0, DebugPointAtBinding.NoneAtLet, matchOn, range0, [matchCase; wildCase], range0)

            let returnTypeInfo = SynBindingReturnInfo.Create duType
            SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr, returnInfo = returnTypeInfo)]
        ]

    let createDuModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) (config: (string * obj) seq) =
        let (SynTypeDefn(synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _range, _trivia)) = typeDefn
        let (SynComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_accessibility, cases, _recordRange), _range) ->

            let ident = LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText))
            let openTarget = SynOpenDeclTarget.ModuleOrNamespace(ident.Lid, range0)
            let openParent = SynModuleDecl.CreateOpen openTarget
            let requiresQualifiedAccess =
                Ast.hasAttribute<RequireQualifiedAccessAttribute> typeDefn
                || config |> Seq.exists (fun (n, v) -> n = "alwaysFullyQualify" && v :?> bool = true)
            
            let toString = createToString requiresQualifiedAccess recordId cases
            let fromString = createFromString requiresQualifiedAccess recordId cases
            let toTag = createToTag requiresQualifiedAccess recordId cases
            let isCase = createIsCase requiresQualifiedAccess recordId cases

            let declarations = [
                openParent
                toString
                fromString
                toTag
                yield! isCase ]

            let info = SynComponentInfo.Create recordId

            let mdl = SynModuleDecl.CreateNestedModule(info,  declarations)
            let dusNamespace =
                config
                |> Seq.tryPick (fun (n,v) -> if n = "namespace" then Some (v :?> string) else None  )
                |> Option.defaultValue "UnknownNamespace"
            SynModuleOrNamespace.CreateNamespace(Ident.CreateLong dusNamespace, isRecursive = true, decls = [mdl])
        | _ -> failwithf "Not a record type"

[<MyriadGenerator("dus")>]
type DUCasesGenerator() =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq {".fs"}
        member _.Generate(context: GeneratorContext) =
            //context.ConfigKey is not currently used but could be a failover config section to use when the attribute passes no config section, or used as a root config
            let ast, _ =
                Ast.fromFilename context.InputFilename
                |> Async.RunSynchronously
                |> Array.head

            let namespaceAndrecords =
                Ast.extractDU ast
                |> List.choose (fun (ns, types) ->
                    match types |> List.filter Ast.hasAttribute<Generator.DuCasesAttribute> with
                    | [] -> None
                    | types -> Some (ns, types))

            let modules =
                namespaceAndrecords
                |> List.collect (fun (ns, dus) ->
                                    dus
                                    |> List.map (fun du -> let config = Generator.getConfigFromAttribute<Generator.DuCasesAttribute> context.ConfigGetter du
                                                           CreateDUModule.createDuModule ns du config))

            Output.Ast modules