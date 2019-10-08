namespace Myriad.Core
open System
open Fantomas
open System.IO
open Microsoft.FSharp.Compiler.Ast
open FsAst

module Ast =
    let createMap (parent: LongIdent) (field: SynField)  =
        let field = field.ToRcd
        let fieldName = match field.Id with None -> failwith "no field name" | Some f -> f 

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let varName = "x"
        let pattern =
            let name = LongIdentWithDots.Create([fieldName.idText])
            let arg =
                let named = SynPatRcd.CreateNamed(Ident.Create varName, SynPatRcd.CreateWild )
                SynPatRcd.CreateTyped(named, recordType)
                |> SynPatRcd.CreateParen

            SynPatRcd.CreateLongIdent(name, [arg])

        let expr =
            let ident = LongIdentWithDots.Create [ yield varName; yield fieldName.idText]
            SynExpr.CreateLongIdent(false, ident, None)

        let valData =
            let argInfo = SynArgInfo.CreateIdString "x"
            let valInfo = SynValInfo.SynValInfo([[argInfo]], SynArgInfo.Empty)
            SynValData.SynValData(None, valInfo, None)

        SynModuleDecl.CreateLet [{SynBindingRcd.Let with
                                    Pattern = pattern
                                    Expr = expr
                                    ValData = valData }]

    let createCreate (parent: LongIdent) (fields: SynFields) =
        let varIdent = LongIdentWithDots.CreateString "create"

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let arguments =
                fields |> List.map (fun f ->let field = f.ToRcd
                                            let name = SynPatRcd.CreateNamed(field.Id.Value, SynPatRcd.CreateWild)
                                            SynPatRcd.CreateTyped(name, field.Type) |> SynPatRcd.CreateParen )

            SynPatRcd.CreateLongIdent(varIdent, arguments)

        let expr = 
            let fields =
                fields
                |> List.map (fun f ->   let field = f.ToRcd
                                        let fieldIdent = match field.Id with None -> failwith "no field name" | Some f -> f 
                                        let name = LongIdentWithDots.Create([fieldIdent.idText])
                                        let ident = SynExpr.CreateIdent fieldIdent
                                        RecordFieldName(name, true), Some ident, None)

            let newRecord = SynExpr.Record(None, None, fields, Microsoft.FSharp.Compiler.Range.range.Zero )
            SynExpr.CreateTyped(newRecord, recordType)

        let returnTypeInfo = SynBindingReturnInfoRcd.Create recordType
        SynModuleDecl.CreateLet [{SynBindingRcd.Let with Pattern = pattern; Expr = expr; ReturnInfo = Some returnTypeInfo }]

    let createRecordModule (data: {| namespaceId: LongIdent; recordId: LongIdent; recordFields : SynFields|}) =

        let openParent = SynModuleDecl.CreateOpen (LongIdentWithDots.Create (data.namespaceId |> List.map (fun ident -> ident.idText)))

        let fieldMaps = data.recordFields |> List.map (createMap data.recordId)
        let create = createCreate data.recordId data.recordFields
        let declarations = [
            yield openParent
            yield!fieldMaps
            yield create ]

        let info = SynComponentInfoRcd.Create data.recordId
        SynModuleDecl.CreateNestedModule(info, declarations)

    let getAst filename =
        let s = File.ReadAllText filename
        let ast = CodeFormatter.Parse(filename, s)
        ast
        
    //{TypeName =
    //     LongIdentWithDots
    //       ([Myriad; Core; MyriadSdkGenerator],
    //        [Library.fs (3,8--3,9) IsSynthetic=false;
    //         Library.fs (3,13--3,14) IsSynthetic=false]);
    //    ArgExpr =
    //     Paren
    //       (Const
    //          (String
    //             ("fields",
    //              Library.fs (3,33--3,41) IsSynthetic=false),
    //           Library.fs (3,33--3,41) IsSynthetic=false),
    //        Library.fs (3,32--3,33) IsSynthetic=false,
    //        Some
    //          Library.fs (3,41--3,42) IsSynthetic=false,
    //        Library.fs (3,32--3,42) IsSynthetic=false);
    //    Target = None;
    //    AppliesToGetterAndSetter = false;
    //    Range =
    //     Library.fs (3,2--3,32) IsSynthetic=false;}
    
    let hasAttribute  (attributeType: Type) (attributeArg: string) (attrib: SynAttribute) =
        let attributeIdent = attributeType.FullName
        let typeNameMatches =
            match attrib.TypeName with
            | LongIdentWithDots(ident, _range) ->
                printfn "attributeIdent:%A -> ident:%A" attributeIdent ident
                let ident =
                    ident
                    |> List.map(fun id -> id.ToString())
                    |> String.concat(".")
                    |> function s -> if s.EndsWith "Attribute" then s else s + "Attribute"
                printfn "attributeIdent:%A -> ident:%A" attributeIdent ident
                ident = attributeIdent
                
        let argumentMatched =
            match attrib.ArgExpr with
            | SynExpr.Paren(p,_,_,_) ->
                match p with
                | SynExpr.Const(c, _r) ->
                    printfn "%A" c
                    match c with
                    | SynConst.String(text,_range) -> text = attributeArg
                    | _ -> false
                | _ -> false
            | _ -> false
                  
        typeNameMatches && argumentMatched        
                  //= typeName && attrib.ArgExpr = attributeArg

        
    let extractRecordMeta ast =
        let records = [
            match ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(name, isScript, qualifiedNameOfFile, scopedPragmas, hashDirectives, modules , g)) ->
                for SynModuleOrNamespace(namespaceIdent, isRecursive, isModule, moduleDecls, preXmlDoc, attributes, access, _) in modules do
                    for moduleDecl in moduleDecls do
                        match moduleDecl with
                        | SynModuleDecl.Types(types, _) ->
                            for TypeDefn( ComponentInfo(attribs, typeParams, constraints, recordIdent, doc, preferPostfix, access1, _), typeDefRepr, memberDefs, _) in types do
                                let hasAttribute =
                                    attribs |> List.tryFind (fun a -> hasAttribute typeof<MyriadSdkGeneratorAttribute> "fields" a)
                                printfn "**hasAttribute: %A" hasAttribute
                                match typeDefRepr with
                                | SynTypeDefnRepr.Exception(a) -> ()
                                | SynTypeDefnRepr.ObjectModel(kind, defs, _) -> ()
                                | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(access2, fields, _), _) ->
                                    yield {|namespaceId = namespaceIdent; recordId = recordIdent; recordFields = fields|}
                                | _ -> ()
                        | _ -> ()
            | _ -> () ]
        records