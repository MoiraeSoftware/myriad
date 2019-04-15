namespace Myriad
open System
open Fantomas
open System.IO
open Microsoft.FSharp.Compiler.Ast
open FsAst
open Argu

module Main =

    type Arguments =
        | [<Mandatory>] InputFile of string
        | Namespace of string
        | [<Mandatory>] OutputFile of string
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputFile _ -> "specify a file to use as input."
                | Namespace _ -> "specify a namespace to use."
                | OutputFile _ -> "Specify the file name that the generated code will be written to."

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

    let createRecordModule (parentId: LongIdent) (fields : SynFields) =
        let fieldMaps = fields |> List.map (createMap parentId)
        let create = createCreate parentId fields
        let declarations = [ yield!fieldMaps; yield create ]
        let info = SynComponentInfoRcd.Create parentId
        SynModuleDecl.CreateNestedModule(info, declarations)

    let getAst filename =
        let s = File.ReadAllText filename
        let ast = CodeFormatter.Parse(filename, s)
        ast

    let extractRecordMeta ast =
        let records = [
            match ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(name, isScript, qualifiedNameOfFile, scopedPragmas, hashDirectives, modules , g)) ->
                for SynModuleOrNamespace(longident, isRecursive, isModule, moduleDecls, preXmlDoc, attributes, access, _) in modules do
                    for moduleDecl in moduleDecls do
                        match moduleDecl with
                        | SynModuleDecl.Types(types, _) ->
                            for TypeDefn( ComponentInfo(attribs, typeParams, constraints, ident, doc, preferPostfix, access1, _), typeDefRepr, memberDefs, _) in types do
                                match typeDefRepr with
                                | SynTypeDefnRepr.Exception(a) -> ()
                                | SynTypeDefnRepr.ObjectModel(kind, defs, _) -> ()
                                | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(access2, fields, _), _) ->
                                    yield (ident, fields)
                                | _ -> ()
                        | _ -> ()
            | _ -> () ]
        records

    [<EntryPoint>]
    let main argv =
        let parser = ArgumentParser.Create<Arguments>(programName = "myriad")

        try
            let results = parser.Parse argv
            let inputFile = results.GetResult InputFile
            let outputFile = results.GetResult OutputFile
            let namespace' =
                match results.TryGetResult Namespace with
                | Some ns -> ns
                | None -> Path.GetFileNameWithoutExtension(inputFile)

            let ast = getAst inputFile
            let records = extractRecordMeta ast
            let modules = 
                records
                |> List.map (fun (p,fs) -> createRecordModule p fs )
    
            let namespace' = 
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace') with IsRecursive = true}
                    .AddDeclarations modules

            let parseTree =
                ParsedInput.CreateImplFile(
                    ParsedImplFileInputRcd.CreateFs(outputFile)
                        .AddModule(namespace')
                )
            let formattedCode = formatAst parseTree
            let code =
                [   "//------------------------------------------------------------------------------"
                    "//        This code was generated by myriad."
                    "//        Changes to this file will be lost when the code is regenerated."
                    "//------------------------------------------------------------------------------"
                    formattedCode ]
                |> String.concat (Environment.NewLine)

            File.WriteAllText(outputFile, code)

            printfn "%A" code
            printfn "AST-----------------------------------------------"
            printfn "%A" parseTree
            0 // return an integer exit code

        with
        | :? ArguParseException as ae ->
            printfn "%s" ae.Message
            match ae.ErrorCode with
            | Argu.ErrorCode.HelpText -> 0
            | _ -> 2
        | :? ArguParseException as ae when ae.ErrorCode = Argu.ErrorCode.HelpText ->
            printfn "%s" ae.Message
            3
        | :? FileNotFoundException as fnf ->
            printfn "ERROR: inputfile %s doesn not exist\n%s" fnf.FileName (parser.PrintUsage())
            4