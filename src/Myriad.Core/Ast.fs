namespace Myriad.Core

open System
open Fantomas
open System.IO
open FSharp.Compiler.Ast
open FSharp.Compiler.SourceCodeServices

module Ast =

    let fromFilename filename =
        let s = File.ReadAllText filename
        let parsingOpts = {FSharpParsingOptions.Default with SourceFiles = [| filename |] }
        let checker = FSharpChecker.Create()
        CodeFormatter.ParseAsync(filename, SourceOrigin.SourceString s, parsingOpts, checker)


    let typeNameMatches (attributeType: Type) (attrib: SynAttribute) =
        match attrib.TypeName with
        | LongIdentWithDots(ident, _range) ->
            let ident =
                ident
                |> List.map(fun id -> id.ToString())
                |> String.concat(".")
                |> function s -> if s.EndsWith "Attribute" then s else s + "Attribute"
            //Support both full name and opened namespace - this is not 100% accurate, and it can't be without full type information, but it should be good enough in practice
            //Replace `+` with `.` - `+` is naming convention for embeded types - in out case it may be attribute type defined inside module.
            if ident.Contains "." then
                attributeType.FullName.Replace("+", ".").EndsWith(ident)
            else
                ident = attributeType.Name


    let hasAttributeWithConst (attributeType: Type) (attributeArg: string) (attrib: SynAttribute) =

        let argumentMatched attrib attributeArg =
            match attrib with
            | SynExpr.Paren(SynExpr.Const(SynConst.String(text,_range), _r),_,_,_) -> text = attributeArg
            | _ -> false

        typeNameMatches attributeType attrib && (argumentMatched attrib.ArgExpr attributeArg)

    let (|HasAttribute|_|) (attributeName: string) (attributes: SynAttributes) =
        attributes
        |> List.collect (fun n -> n.Attributes)
        |> List.tryFind (hasAttributeWithConst typeof<MyriadGeneratorAttribute> attributeName)

    let extractTypeDefn (ast: ParsedInput) =
        [ match ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(_name, _isScript, _qualifiedNameOfFile, _scopedPragmas, _hashDirectives, modules, _g)) ->
                for SynModuleOrNamespace(_namespaceId, _isRecursive, _isModule, moduleDecls, _preXmlDoc, _attributes, _access, _) as ns in modules do
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

    let hasAttributeWithName<'a> (attributeName: string) (TypeDefn(ComponentInfo(attributes, _typeParams, _constraints, _recordIdent, _doc, _preferPostfix, _access, _), _typeDefRepr, _memberDefs, _))  =
        attributes
        |> List.exists (fun n -> n.Attributes |> List.exists (hasAttributeWithConst typeof<'a> attributeName))

    let hasAttribute<'a>  (TypeDefn(ComponentInfo(attributes, _typeParams, _constraints, _recordIdent, _doc, _preferPostfix, _access, _), _typeDefRepr, _memberDefs, _))  =
        attributes
        |> List.collect (fun n -> n.Attributes)
        |> List.exists (typeNameMatches typeof<'a>)

    let getAttribute<'a>  (TypeDefn(ComponentInfo(attributes, _typeParams, _constraints, _recordIdent, _doc, _preferPostfix, _access, _), _typeDefRepr, _memberDefs, _))  =
        attributes
        |> List.collect (fun n -> n.Attributes)
        |> List.tryFind (typeNameMatches typeof<'a>)