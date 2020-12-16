namespace Myriad.Plugins

open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core

module internal CreateDUModule =
    open FSharp.Compiler.Range

    let createToString (parent: LongIdent) (cases: SynUnionCase list) =
        let varIdent = LongIdentWithDots.CreateString "toString"
        let inputIdent = "x"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let ident = Ident(inputIdent, range.Zero)
            let name = SynPatRcd.CreateNamed(ident, SynPatRcd.CreateWild)
            let args = SynPatRcd.CreateTyped(name, duType) |> SynPatRcd.CreateParen
            SynPatRcd.CreateLongIdent(varIdent, [args])

        let expr =
            let matches =
                cases
                |> List.map (fun c ->
                    let case = c.ToRcd
                    let indent = LongIdentWithDots.CreateString (case.Id.idText)
                    let args = if case.HasFields then [SynPatRcd.CreateWild] else []
                    let p = SynPatRcd.CreateLongIdent(indent, args)
                    let rhs =
                       SynExpr.Const(SynConst.CreateString case.Id.idText, range.Zero)
                    SynMatchClause.Clause(p.FromRcd, None, rhs, range.Zero, DebugPointForTarget.Yes )
                )
            let matchOn =
                let ident = LongIdentWithDots.CreateString inputIdent
                SynExpr.CreateLongIdent(false, ident, None)

            SynExpr.Match(DebugPointForBinding.NoDebugPointAtInvisibleBinding , matchOn, matches, range.Zero)

        let returnTypeInfo = SynBindingReturnInfoRcd.Create duType
        SynModuleDecl.CreateLet [{SynBindingRcd.Let with Pattern = pattern; Expr = expr; ReturnInfo = Some returnTypeInfo }]

    let createFromString (parent: LongIdent) (cases: SynUnionCase list) =
        let varIdent = LongIdentWithDots.CreateString "fromString"
        let inputIdent = "x"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let inputType =
            LongIdentWithDots.CreateString "string"
            |> SynType.CreateLongIdent

        let pattern =
            let ident = Ident(inputIdent, range.Zero)
            let name = SynPatRcd.CreateNamed(ident, SynPatRcd.CreateWild)
            let args = SynPatRcd.CreateTyped(name, inputType) |> SynPatRcd.CreateParen
            SynPatRcd.CreateLongIdent(varIdent, [args])

        let expr =
            let matches =
                cases
                //Only provide `fromString` for cases with no fields
                |> List.filter (fun c -> not (c.ToRcd.HasFields))
                |> List.map (fun c ->
                    let case = c.ToRcd
                    let rcd = {SynPatConstRcd.Const = SynConst.CreateString case.Id.idText; Range = range.Zero }
                    let p = SynPatRcd.Const (rcd)
                    let rhs =
                        let f = SynExpr.Ident (Ident("Some", range.Zero))
                        let x = SynExpr.Ident (Ident(case.Id.idText, range.Zero))
                        SynExpr.App(ExprAtomicFlag.NonAtomic, false, f, x, range.Zero)
                    SynMatchClause.Clause(p.FromRcd, None, rhs, range.Zero, DebugPointForTarget.No )
                )
            let wildCase =
                let rhs = SynExpr.Ident (Ident("None", range.Zero))
                SynMatchClause.Clause(SynPat.Wild (range.Zero), None, rhs, range.Zero, DebugPointForTarget.No)

            let matchOn =
                let ident = LongIdentWithDots.CreateString inputIdent
                SynExpr.CreateLongIdent(false, ident, None)

            SynExpr.Match(DebugPointForBinding.NoDebugPointAtLetBinding, matchOn, [yield! matches; wildCase], range.Zero)

        let returnTypeInfo = SynBindingReturnInfoRcd.Create duType
        SynModuleDecl.CreateLet [{SynBindingRcd.Let with Pattern = pattern; Expr = expr; ReturnInfo = Some returnTypeInfo }]

    let createToTag (parent: LongIdent) (cases: SynUnionCase list) =
        let varIdent = LongIdentWithDots.CreateString "toTag"
        let inputIdent = "x"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let ident = Ident(inputIdent, range.Zero)
            let name = SynPatRcd.CreateNamed(ident, SynPatRcd.CreateWild)
            let args = SynPatRcd.CreateTyped(name, duType) |> SynPatRcd.CreateParen
            SynPatRcd.CreateLongIdent(varIdent, [args])

        let expr =
            let matches =
                cases
                |> List.mapi (fun i c ->
                    let case = c.ToRcd
                    let indent = LongIdentWithDots.CreateString (case.Id.idText)
                    let args = if case.HasFields then [SynPatRcd.CreateWild] else []
                    let p = SynPatRcd.CreateLongIdent(indent, args)
                    let rhs =
                       SynExpr.Const(SynConst.Int32 i, range.Zero)
                    SynMatchClause.Clause(p.FromRcd, None, rhs, range.Zero, DebugPointForTarget.No )
                )
            let matchOn =
                let ident = LongIdentWithDots.CreateString inputIdent
                SynExpr.CreateLongIdent(false, ident, None)

            SynExpr.Match(DebugPointForBinding.NoDebugPointAtLetBinding , matchOn, matches, range.Zero)

        let returnTypeInfo = SynBindingReturnInfoRcd.Create duType
        SynModuleDecl.CreateLet [{SynBindingRcd.Let with Pattern = pattern; Expr = expr; ReturnInfo = Some returnTypeInfo }]

    let createIsCase (parent: LongIdent) (cases: SynUnionCase list) =
        [ for c in cases do
            let case = c.ToRcd
            let varIdent = LongIdentWithDots.CreateString (sprintf "is%s" case.Id.idText)
            let inputIdent = "x"

            let duType =
                LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
                |> SynType.CreateLongIdent

            let pattern =
                let ident = Ident(inputIdent, range.Zero)
                let name = SynPatRcd.CreateNamed(ident, SynPatRcd.CreateWild)
                let args = SynPatRcd.CreateTyped(name, duType) |> SynPatRcd.CreateParen
                SynPatRcd.CreateLongIdent(varIdent, [args])

            let expr =
                let matchCase =
                    let indent = LongIdentWithDots.CreateString (case.Id.idText)
                    let args = if case.HasFields then [SynPatRcd.CreateWild] else []
                    let p = SynPatRcd.CreateLongIdent(indent, args)

                    let rhs = SynExpr.Const (SynConst.Bool true, range.Zero)
                    SynMatchClause.Clause(p.FromRcd, None, rhs, range.Zero, DebugPointForTarget.No)

                let wildCase =
                    let rhs = SynExpr.Const (SynConst.Bool false, range.Zero)
                    SynMatchClause.Clause(SynPat.Wild (range.Zero), None, rhs, range.Zero, DebugPointForTarget.No)

                let matchOn =
                    let ident = LongIdentWithDots.CreateString inputIdent
                    SynExpr.CreateLongIdent(false, ident, None)

                SynExpr.Match(NoDebugPointAtLetBinding, matchOn, [matchCase; wildCase], range.Zero)

            let returnTypeInfo = SynBindingReturnInfoRcd.Create duType
            SynModuleDecl.CreateLet [{SynBindingRcd.Let with Pattern = pattern; Expr = expr; ReturnInfo = Some returnTypeInfo }]
        ]


    let createDuModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_accessibility, cases, _recordRange), _range) ->

            let openParent = SynModuleDecl.CreateOpen (LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText)))

            let toString = createToString recordId cases
            let fromString = createFromString recordId cases
            let toTag = createToTag recordId cases
            let iss = createIsCase recordId cases

            let declarations = [
                openParent
                toString
                fromString
                toTag
                yield! iss ]

            let info = SynComponentInfoRcd.Create recordId
            SynModuleDecl.CreateNestedModule(info, declarations)
        | _ -> failwithf "Not a record type"



[<MyriadGenerator("DUs")>]
type DUCasesGenerator() =

    interface IMyriadGenerator with
        member __.Generate(namespace', inputFile: string) =
            let ast =
                Ast.fromFilename inputFile
                |> Async.RunSynchronously
                |> Array.head
                |> fst
            let namespaceAndRecords = Ast.extractDU ast
            let modules =
                namespaceAndRecords
                |> List.collect (fun (ns, dus) ->
                                    dus
                                    |> List.filter (Ast.hasAttribute<Generator.DuCasesAttribute>)
                                    |> List.map (CreateDUModule.createDuModule ns))

            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = modules }

            namespaceOrModule