namespace Myriad.Plugins

open System
open FSharp.Compiler.Ast
open FsAst
open Myriad.Core

module internal CreateDUModule =
    open FSharp.Compiler.Range

    let createToString (parent: LongIdent) (cases: SynUnionCases) =
        let varIdent = LongIdentWithDots.CreateString "toString"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let ident = Ident("currency", range.Zero)
            let name = SynPatRcd.CreateNamed(ident, SynPatRcd.CreateWild)
            let args = SynPatRcd.CreateTyped(name, duType) |> SynPatRcd.CreateParen
            SynPatRcd.CreateLongIdent(varIdent, [args])

        let expr =
            let matches =
                cases
                |> List.map (fun c ->
                    let case = c.ToRcd
                    let indent = LongIdentWithDots.CreateString (case.Id.idText)
                    let p = SynPatRcd.CreateLongIdent(indent, [])
                    let rhs =
                       SynExpr.Const(SynConst.CreateString case.Id.idText, range.Zero)
                    SynMatchClause.Clause(p.FromRcd, None, rhs, range.Zero, SequencePointInfoForTarget.SequencePointAtTarget )
                )
            let matchOn =
                let ident = LongIdentWithDots.CreateString "currency"
                SynExpr.CreateLongIdent(false, ident, None)

            SynExpr.Match(SequencePointInfoForBinding.NoSequencePointAtLetBinding, matchOn, matches, range.Zero)

        let returnTypeInfo = SynBindingReturnInfoRcd.Create duType
        SynModuleDecl.CreateLet [{SynBindingRcd.Let with Pattern = pattern; Expr = expr; ReturnInfo = Some returnTypeInfo }]

    let createFromString (parent: LongIdent) (cases: SynUnionCases) =
        let varIdent = LongIdentWithDots.CreateString "fromString"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let inputType =
            LongIdentWithDots.CreateString "string"
            |> SynType.CreateLongIdent

        let pattern =
            let ident = Ident("currencyString", range.Zero)
            let name = SynPatRcd.CreateNamed(ident, SynPatRcd.CreateWild)
            let args = SynPatRcd.CreateTyped(name, inputType) |> SynPatRcd.CreateParen
            SynPatRcd.CreateLongIdent(varIdent, [args])

        let expr =
            let matches =
                cases
                |> List.map (fun c ->
                    let case = c.ToRcd
                    let rcd = {SynPatConstRcd.Const = SynConst.CreateString case.Id.idText; Range = range.Zero }
                    let p = SynPatRcd.Const (rcd)
                    let rhs =
                        let f = SynExpr.Ident (Ident("Some", range.Zero))
                        let x = SynExpr.Ident (Ident(case.Id.idText, range.Zero))
                        SynExpr.App(ExprAtomicFlag.NonAtomic, false, f, x, range.Zero)
                    SynMatchClause.Clause(p.FromRcd, None, rhs, range.Zero, SequencePointInfoForTarget.SequencePointAtTarget )
                )
            let wildCase =
                let rhs = SynExpr.Ident (Ident("None", range.Zero))
                SynMatchClause.Clause(SynPat.Wild (range.Zero), None, rhs, range.Zero, SequencePointInfoForTarget.SequencePointAtTarget)

            let matchOn =
                let ident = LongIdentWithDots.CreateString "currencyString"
                SynExpr.CreateLongIdent(false, ident, None)

            SynExpr.Match(SequencePointInfoForBinding.NoSequencePointAtLetBinding, matchOn, [yield! matches; wildCase], range.Zero)

        let returnTypeInfo = SynBindingReturnInfoRcd.Create duType
        SynModuleDecl.CreateLet [{SynBindingRcd.Let with Pattern = pattern; Expr = expr; ReturnInfo = Some returnTypeInfo }]


    let createDuModule (moduleOrNamespace: SynModuleOrNamespace) (typeDefn: SynTypeDefn) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (SynModuleOrNamespace(namespaceId, _isRecursive, _isModule, _moduleDecls, _preXmlDoc, _attributes, _access, _)) = moduleOrNamespace
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_accessibility, cases, _recordRange), _range) ->

            let openParent = SynModuleDecl.CreateOpen (LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText)))

            let toString = createToString recordId cases
            let fromString = createFromString recordId cases

            let declarations = [
                openParent
                toString
                fromString ]

            let info = SynComponentInfoRcd.Create recordId
            SynModuleDecl.CreateNestedModule(info, declarations)
        | _ -> failwithf "Not a record type"



[<MyriadGenerator("DUs")>]
type DUCasesGenerator() =

    interface IMyriadGenerator with
        member __.Generate(namespace', ast: ParsedInput) =
            let namespaceAndrecords = Ast.extractDU ast
            let modules =
                namespaceAndrecords
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