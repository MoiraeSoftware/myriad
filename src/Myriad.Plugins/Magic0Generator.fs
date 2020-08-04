namespace Myriad.Plugins

open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core

module internal Magic0Module =
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

    let createUnionRec (recordId:LongIdent) (cases:SynUnionCase list)=
        let fields=seq{
                for x in cases do
                    let case=x.ToRcd
                    if case.HasFields then
                        match case.Type with
                        | UnionCaseFields(cs) ->
                                    printfn "==================================================="
                                    printfn "%A=%A" cs.Length cs
                                    if 1=cs.Length then
                                       let field= cs.[0].ToRcd
                                       let field={field with Id=Some case.Id}
                                       yield field
                                    if 1<cs.Length then
                                        let flds=[for i in 0..cs.Length-2 -> (false, cs.[i].ToRcd.Type)]
                                        let flds=flds@[(true, cs.[cs.Length-1].ToRcd.Type)]
                                        let field= cs.[0].ToRcd
                                        let field={field with Id=Some case.Id; Type = SynType.Tuple(false,flds, range0)}
                                        yield field
                    else
                        ()
            }

        printfn "%A" fields

        let fields=Seq.toList fields

        let rc=SynTypeDefnSimpleReprRecordRcd.Create fields
        let lid=LongIdentWithDots.CreateFromLongIdent(recordId).AsString + "Record"
        printfn "%A" lid
        let ci=SynComponentInfoRcd.Create(Ident.CreateLong lid)
        SynModuleDecl.CreateSimpleType(ci,SynTypeDefnSimpleReprRcd.Record rc)
        
    let createKindFun (recordId:LongIdent) (cases:SynUnionCase list)=
        let clauses= cases|>List.map (fun x ->
            let case=x.ToRcd
            let args = if case.HasFields then [ SynPatRcd.CreateWild ] else []

            let pat= SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateFromLongIdent(recordId@[case.Id]), args)
            let lid=LongIdentWithDots.CreateFromLongIdent(recordId).AsString + "Kind." 
            let lid=lid + (case.Id.ToString())
            let exp=SynExpr.CreateLongIdent(LongIdentWithDots.CreateString lid )            
            SynMatchClause.Clause (pat.FromRcd, None, exp, range0, DebugPointForTarget.No)
        ) 

        let mexp=SynExpr.CreateMatch(SynExpr.CreateIdent(Ident("this",range0)),clauses)

        let bind=SynBindingRcd.Null
        let bind={bind with Expr=mexp}
        let bind={bind with Pattern=SynPatRcd.CreateLongIdent(LongIdentWithDots.Create ["this";"kind"],[])}


        let ci=SynComponentInfoRcd.Create(recordId)
        let mbr=SynMemberDefn.CreateMember bind
        let obm=SynTypeDefnReprObjectModelRcd.Create []

        let obm={obm with Kind=SynTypeDefnKind.TyconAugmentation}
        printfn "%A" obm

        let typ=TypeDefn(ci.FromRcd,obm.FromRcd,[mbr],range0)
        SynModuleDecl.Types([typ],range0)
        


    let createKindDU (recordId:LongIdent) (cases:SynUnionCase list)=

        let kindcases = cases |>List.map ( fun x ->
               let x=x.ToRcd
               SynUnionCaseRcd.Create(x.Id, SynUnionCaseType.Create([]))
            )
        let un=SynTypeDefnSimpleReprUnionRcd.Create kindcases
        let lid=LongIdentWithDots.CreateFromLongIdent(recordId).AsString + "Kind"
        printfn "%A" lid
        let ci=SynComponentInfoRcd.Create(Ident.CreateLong lid)
        SynModuleDecl.CreateSimpleType(ci,SynTypeDefnSimpleReprRcd.Union un) 


    let createDuModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) =
        let (TypeDefn(synComponentInfo, synTypeDefnRepr, _members, _range)) = typeDefn
        let (ComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo
        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_accessibility, cases, _recordRange), _range) ->

            let openParent = SynModuleDecl.CreateOpen (LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText)))


            let kindDU = createKindDU recordId cases
            let kindFun= createKindFun recordId cases
            let UnionRec=createUnionRec recordId cases
            // let toString = createToString recordId cases
            // let fromString = createFromString recordId cases
            // let toTag = createToTag recordId cases
            // let iss = createIsCase recordId cases

            let declarations = [
                openParent
                kindDU
                kindFun
                UnionRec
                ]

            let info = SynComponentInfoRcd.Create recordId
            SynModuleDecl.CreateNestedModule(info, declarations)
        | _ -> failwithf "Not a record type"



[<MyriadGenerator("Magic0")>]
type Magic0Generator() =

    interface IMyriadGenerator with
        member __.Generate(namespace', ast: ParsedInput) =
            let namespaceAndRecords = Ast.extractDU ast
            let modules =
                namespaceAndRecords
                |> List.collect (fun (ns, dus) ->
                                    dus
                                    |> List.filter (Ast.hasAttribute<Generator.Magic0Attribute>)
                                    |> List.map (Magic0Module.createDuModule ns))

            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = modules }

            namespaceOrModule