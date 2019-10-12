namespace Myriad.Core
open System
open Fantomas
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fantomas
open FsAst

module Ast =

    let fromFilename filename =
        let s = File.ReadAllText filename
        let ast = CodeFormatter.Parse(filename, s)
        ast

    let hasAttributeWithConst  (attributeType: Type) (attributeArg: string) (attrib: SynAttribute) =
        let typeNameMatches (attributeType: Type) (attrib: SynAttribute) =
            match attrib.TypeName with
            | LongIdentWithDots(ident, _range) ->
                let ident =
                    ident
                    |> List.map(fun id -> id.ToString())
                    |> String.concat(".")
                    |> function s -> if s.EndsWith "Attribute" then s else s + "Attribute"
                ident = attributeType.FullName

        let argumentMatched attrib attributeArg =
            match attrib with
            | SynExpr.Paren(SynExpr.Const(SynConst.String(text,_range), _r),_,_,_) -> text = attributeArg
            | _ -> false

        typeNameMatches attributeType attrib && (argumentMatched attrib.ArgExpr attributeArg)

    let (|HasFieldsAttribute|_|) (attributes: SynAttributes) =
        attributes |> List.tryFind (hasAttributeWithConst typeof<MyriadSdkGeneratorAttribute> "fields")

    let extractTypeDefn (ast: ParsedInput) =
        [ match ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(name, isScript, qualifiedNameOfFile, scopedPragmas, hashDirectives, modules, g)) ->
                for SynModuleOrNamespace(namespaceId, _isRecursive, _isModule, moduleDecls, _preXmlDoc, _attributes, _access, _) as ns in modules do
                    for moduleDecl in moduleDecls do
                        match moduleDecl with
                        | SynModuleDecl.Types(types, _) -> yield (ns, types)
                        | _ -> ()
            | _ -> () ]

    let isRecord (TypeDefn(_componentInfo, typeDefRepr, _memberDefs, _)) =
        match typeDefRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record _, _) -> true
        | _ -> false

    let extractRecords (ast: ParsedInput) =

        let records =
            let types = extractTypeDefn ast
            let onlyRecords =
                types
                |> List.map (fun (ns, types) -> ns, types |> List.filter isRecord )
            onlyRecords

        records

    let hasFieldsAttribute (TypeDefn(ComponentInfo(attributes, _typeParams, _constraints, _recordIdent, _doc, _preferPostfix, _access, _), _typeDefRepr, _memberDefs, _)) =
        attributes
        |> List.exists (hasAttributeWithConst typeof<MyriadSdkGeneratorAttribute> "fields")