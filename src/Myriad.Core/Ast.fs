namespace Myriad.Core

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.CodeAnalysis
open Fantomas
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open FSharp.Compiler.SyntaxTrivia

module DynamicReflection =
    open System.Reflection
    open Microsoft.FSharp.Reflection

    // Various flags that specify what members can be called 
    // NOTE: Remove 'BindingFlags.NonPublic' if you want a version
    // that can call only public methods of classes
    let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static 
    let instanceFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
    let private ctorFlags = instanceFlags
    let inline asMethodBase(a:#MethodBase) = a :> MethodBase

    // The operator takes just instance and a name. Depending on how it is used
    // it either calls method (when 'R is function) or accesses a property
    let (?) (o:obj) name : 'R =
      // The return type is a function, which means that we want to invoke a method
      if FSharpType.IsFunction(typeof<'R>) then

        // Get arguments (from a tuple) and their types
        let argType, resType = FSharpType.GetFunctionElements(typeof<'R>)
        // Construct an F# function as the result (and cast it to the
        // expected function type specified by 'R)
        FSharpValue.MakeFunction(typeof<'R>, fun args ->
          
          // We treat elements of a tuple passed as argument as a list of arguments
          // When the 'o' object is 'System.Type', we call static methods
          let methods, instance, args = 
            let args = 
              // If argument is unit, we treat it as no arguments,
              // if it is not a tuple, we create singleton array,
              // otherwise we get all elements of the tuple
              if argType = typeof<unit> then [| |]
              elif not(FSharpType.IsTuple(argType)) then [| args |]
              else FSharpValue.GetTupleFields(args)

            // Static member call (on value of type System.Type)?
            if (typeof<System.Type>).IsAssignableFrom(o.GetType()) then 
              let methods = (unbox<Type> o).GetMethods(staticFlags) |> Array.map asMethodBase
              let ctors = (unbox<Type> o).GetConstructors(ctorFlags) |> Array.map asMethodBase
              Array.concat [ methods; ctors ], null, args
            else 
              o.GetType().GetMethods(instanceFlags) |> Array.map asMethodBase, o, args
            
          // A simple overload resolution based on the name and the number of parameters only
          // TODO: This doesn't correctly handle multiple overloads with same parameter count
          let methods = 
            [ for m in methods do
                if m.Name = name && m.GetParameters().Length = args.Length then yield m ]
            
          // If we find suitable method or constructor to call, do it!
          match methods with 
          | [] -> failwithf $"No method '%s{name}' with %d{args.Length} arguments found"
          | _::_::_ -> failwithf $"Multiple methods '%s{name}' with %d{args.Length} arguments found"
          | [:? ConstructorInfo as c] -> c.Invoke(args)
          | [ m ] -> m.Invoke(instance, args) ) |> unbox<'R>

      else
        // The result type is not an F# function, so we're getting a property
        // When the 'o' object is 'System.Type', we access static properties
        let typ, flags, instance = 
          if (typeof<System.Type>).IsAssignableFrom(o.GetType()) 
            then unbox o, staticFlags, null
            else o.GetType(), instanceFlags, o
          
        // Find a property that we can call and get the value
        let prop = typ.GetProperty(name, flags)
        if prop = null && instance = null then 
          // The syntax can be also used to access nested types of a type
          let nested = typ.Assembly.GetType(typ.FullName + "+" + name)
          // Return nested type if we found one
          if nested = null then 
            failwithf $"Property or nested type '%s{name}' not found in '%s{typ.Name}'." 
          elif not ((typeof<'R>).IsAssignableFrom(typeof<System.Type>)) then
            let rname = (typeof<'R>.Name)
            failwithf $"Cannot return nested type '%s{nested.Name}' as a type '%s{rname}'."
          else nested |> box |> unbox<'R>
        else
          // Call property and return result if we found some
          let meth = prop.GetGetMethod(true)
          if prop = null then failwithf $"Property '%s{name}' found, but doesn't have 'get' method."
          try meth.Invoke(instance, [| |]) |> unbox<'R>
          with _ -> failwithf $"Failed to get value of '%s{name}' property (of type '%s{typ.Name}')"

module Ast =
    
    let fromFilename filename =
        let allLines = Generation.linesToKeep filename |> String.concat Environment.NewLine
        let parsingOpts = {FSharpParsingOptions.Default with
                               SourceFiles = [| filename |]
                               ErrorSeverityOptions = FSharpDiagnosticOptions.Default }
        let checker = FSharpChecker.Create()
        CodeFormatter.ParseAsync(filename, SourceOrigin.SourceString allLines, parsingOpts, checker)

    let typeNameMatches (attributeType: Type) (attrib: SynAttribute) =
        match attrib.TypeName with
        | LongIdentWithDots(ident, _range) ->
            let ident =
                ident
                |> List.map(fun id -> id.ToString())
                |> String.concat(".")
                |> function s -> if s.EndsWith "Attribute" then s else s + "Attribute"
            //Support both full name and opened namespace - this is not 100% accurate, and it can't be without full type information, but it should be good enough in practice
            //Replace `+` with `.` - `+` is naming convention for embeded types - in our case it may be an attribute type defined inside module.
            if ident.Contains "." then
                attributeType.FullName.Replace("+", ".").EndsWith(ident)
            else
                ident = attributeType.Name

    let getAttributeConstants (attrib: SynAttribute) =
        let (|StringConst|_|) = function
            | SynExpr.Const( SynConst.String(text,_,_), _) -> Some text
            | _ -> None

        match attrib.ArgExpr with
        | SynExpr.Paren(StringConst text,_,_,_) -> [text]
        | SynExpr.Paren(SynExpr.Tuple(_,entries,_,_),_,_,_) -> entries |> List.choose (|StringConst|_|)
        | StringConst text -> [text]
        | _ -> []

    let hasAttributeWithConst (attributeType: Type) (attributeArg: string) (attrib: SynAttribute) =

        let argumentMatched attributeArg =
            match getAttributeConstants attrib with
            | [] -> false
            | t -> t  |> List.contains attributeArg

        typeNameMatches attributeType attrib && (argumentMatched attributeArg)

    type SynComponentInfo with
        member x.attributes =
            let (SynComponentInfo(attributes, _typeParams, _constraints, _recordIdent, _doc, _preferPostfix, _access, _ciRange)) = x
            attributes

    let (|HasAttribute|_|) (attributeName: string) (attributes: SynAttributes) =
        attributes
        |> List.collect (fun n -> n.Attributes)
        |> List.tryFind (hasAttributeWithConst typeof<MyriadGeneratorAttribute> attributeName)

    let hasAttributeWithName<'a> (attributeName: string) (SynTypeDefn(synComponentInfo,  _typeDefRepr, _memberDefs, _implicitCtor,_range ,_trivia))  =
        synComponentInfo.attributes
        |> List.exists (fun n -> n.Attributes |> List.exists (hasAttributeWithConst typeof<'a> attributeName))

    let hasAttribute<'a> (SynTypeDefn(synComponentInfo,  _typeDefRepr, _memberDefs, _implicitCtor,_range ,_trivia))  =
        synComponentInfo.attributes
        |> List.collect (fun n -> n.Attributes)
        |> List.exists (typeNameMatches typeof<'a>)

    let getAttribute<'a> (SynTypeDefn(synComponentInfo,  _typeDefRepr, _memberDefs, _implicitCtor,_range ,_trivia))  =
        synComponentInfo.attributes
        |> List.collect (fun n -> n.Attributes)
        |> List.tryFind (typeNameMatches typeof<'a>)

    let extractTypeDefn (ast: ParsedInput) =
        let rec extractTypes (moduleDecls: SynModuleDecl list) (ns: LongIdent) =
            [   for moduleDecl in moduleDecls do
                    match moduleDecl with
                    | SynModuleDecl.Types(types, _) ->
                        yield (ns, types)
                    | SynModuleDecl.NestedModule(SynComponentInfo(_, _, _, longId, _, _, _, _), _, decls, _, _, _) ->
                        let combined = longId |> List.append ns
                        yield! (extractTypes decls combined)
                    | other -> ()
            ]

        [   match ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(_name, _isScript, _qualifiedNameOfFile, _scopedPragmas, _hashDirectives, modules, _g)) ->
                for SynModuleOrNamespace(namespaceId, _isRec, _isModule, moduleDecls, _preXmlDoc, _attributes, _access, _) as ns in modules do
                    yield! extractTypes moduleDecls namespaceId
            | _ -> () ]

    let isRecord (SynTypeDefn(_componentInfo, typeDefRepr, _memberDefs,_,_,_)) =
        match typeDefRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record _, _) -> true
        | _ -> false

    let isDu (SynTypeDefn(_componentInfo, typeDefRepr, _memberDefs,_,_,_)) =
        match typeDefRepr with
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union _, _) -> true
        | _ -> false

    let extractRecords (ast: ParsedInput) =
        let types = extractTypeDefn ast
        let onlyRecords =
            types
            |> List.map (fun (ns, types) -> ns, types |> List.filter isRecord )
        onlyRecords

    let extractDU (ast: ParsedInput) =
        let types = extractTypeDefn ast
        let onlyDus =
            types
            |> List.map (fun (ns, types) -> ns, types |> List.filter isDu )
        onlyDus
        
        
    module ModuleOrNamespace =
        let hasAttribute<'a>
            (SynModuleOrNamespace (_namespaceId, _isRec, _isModule, _moduleDecls, _preXmlDoc, attributes, _access, _range)) =
            attributes
            |> List.collect (fun n -> n.Attributes)
            |> List.exists (typeNameMatches typeof<'a>)
            
        let modulesWithAttribute<'a> (ast: ParsedInput) =
            [ match ast with
              | ParsedInput.ImplFile (ParsedImplFileInput (_name, _isScript, _qualifiedNameOfFile, _scopedPragmas, _hashDirectives, modules, _g)) ->
                  for SynModuleOrNamespace (_namespaceId, _isRec, moduleOrNs, _moduleDecls, _preXmlDoc, _attributes, _access, _range) as ns in modules do
                      if moduleOrNs.IsModule && hasAttribute<'a> ns then
                          yield ns
              | _ -> () ]
            
        let getTypeDefns (nsOrModule: SynModuleOrNamespace) =
            let rec extractTypes (moduleDecls: SynModuleDecl list) (ns: LongIdent) =
                [ for moduleDecl in moduleDecls do
                      match moduleDecl with
                      | SynModuleDecl.Types (types, _) -> yield (ns, types)
                      | SynModuleDecl.NestedModule (SynComponentInfo (_attribs, _typeParams, _constraints, longId, _xmlDoc, _preferPostfix, _accessibility, _range), _isRec, decls, _local, _outerRange,_trivia) ->
                          let combined = longId |> List.append ns
                          yield! (extractTypes decls combined)
                      | _other -> () ]

            let (SynModuleOrNamespace (namespaceId, _isRec, _isModule, moduleDecls, _preXmlDoc, _attributes, _access, _range)) =
                nsOrModule

            extractTypes moduleDecls namespaceId
        
        let records (nsOrModule: SynModuleOrNamespace) =
            let types = getTypeDefns nsOrModule

            let onlyRecords =
                types
                |> List.map (fun (ns, types) -> ns, types |> List.filter isRecord)

            onlyRecords

        let dus (nsOrModule: SynModuleOrNamespace) =
            let types = getTypeDefns nsOrModule

            let onlyDus =
                types
                |> List.map (fun (ns, types) -> ns, types |> List.filter isDu)

            onlyDus
        
        let recordsOrDus (nsOrModule: SynModuleOrNamespace) =
            let types = getTypeDefns nsOrModule

            let recordsOrDus =
                types
                |> List.map (fun (ns, types) -> ns, types |> List.filter (fun t -> t |> isDu || t |> isRecord))

            recordsOrDus

    module Ident =
        let asCamelCase (ident: Ident) =
            Ident.Create(ident.idText.Substring(0, 1).ToLowerInvariant() + ident.idText.Substring(1))
            
    type  ParsedImplFileInput with
        static member CreateFs(name, ?isScript, ?scopedPragmas, ?hashDirectives, ?modules, ?isLastCompiland, ?isExe) =
            let file = $"%s{name}.fs"
            let isScript = defaultArg isScript false
            let qualName = QualifiedNameOfFile.Create name
            let scopedPragmas = defaultArg scopedPragmas []
            let hashDirectives = defaultArg hashDirectives []
            let modules = defaultArg modules []
            let isLastCompiland = defaultArg isLastCompiland true
            let isExe = defaultArg isExe false
            ParsedImplFileInput(file, isScript, qualName, scopedPragmas, hashDirectives, modules, (isLastCompiland, isExe))
            
    type SynModuleOrNamespace with
        static member CreateNamespace(ident, ?isRecursive, ?decls, ?docs, ?attribs, ?access) =
            let range = range0
            let kind = SynModuleOrNamespaceKind.DeclaredNamespace
            let isRecursive = defaultArg isRecursive false
            let decls = defaultArg decls []
            let docs = defaultArg docs  PreXmlDoc.Empty
            let attribs = defaultArg attribs SynAttributes.Empty
            SynModuleOrNamespace(ident, isRecursive, kind, decls, docs, attribs, access, range)
            
    type SynComponentInfo with
        static member Create(id: LongIdent, ?attributes, ?parameters, ?constraints, ?xmldoc, ?preferPostfix, ?access) =
            let attributes = defaultArg attributes SynAttributes.Empty
            let constraints = defaultArg constraints []
            let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
            let preferPostfix = defaultArg preferPostfix false
            let access = defaultArg access None
            let range = range0
            SynComponentInfo(attributes, parameters, constraints,id, xmldoc,preferPostfix, access, range)                    
            
    type SynPat with
        static member CreateNamed(ident, ?isThisVal, ?access) =
            let isThisVal = defaultArg isThisVal false
            SynPat.Named(ident, isThisVal, access, range0)
            
        static member CreateWild =
            SynPat.Wild(range0)
            
        static member CreateTyped(pat, typ) =
            SynPat.Typed(pat, typ, range0)
                         
        static member CreateParen(exp) =
            SynPat.Paren(exp, range0)
            
        static member CreateLongIdent(id, args, ?prop, ?typarDecls, ?extraId, ?access) =
            let args = SynArgPats.Pats(args)
            SynPat.LongIdent(id, prop, extraId, typarDecls, args, access, range0)
            
        static member CreateNull =
            SynPat.Null(range0)
            
        static member CreateConst(expr) =
            SynPat.Const(expr, range0)

    type SynBinding with
        static member Let(?access, ?isInline, ?isMutable, ?attributes, ?xmldoc, ?valData, ?pattern, ?returnInfo, ?expr) =
            let isInline = defaultArg isInline false
            let isMutable = defaultArg isMutable false
            let attributes = defaultArg attributes SynAttributes.Empty
            let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
            let valData = defaultArg valData (SynValData(None, SynValInfo([], SynArgInfo.Empty), None))
            let headPat = defaultArg pattern SynPat.CreateNull
            let expr = defaultArg expr (SynExpr.CreateTyped(SynExpr.CreateNull, SynType.CreateUnit))
            let bind = DebugPointAtBinding.NoneAtLet
            let trivia = { LetKeyword = Some range0
                           EqualsRange = Some range0 }
            SynBinding.SynBinding(access, SynBindingKind.Normal, isInline, isMutable, attributes, xmldoc, valData, headPat, returnInfo, expr, range0, bind, trivia)
            
            
    type SynModuleDecl with
        static member CreateLet(bindings, ?isRecursive) =
            let isRecursive = defaultArg isRecursive false
            SynModuleDecl.Let(isRecursive, bindings, range0)
            
        static member CreateNestedModule(ci, decls, ?isRec, ?isCont) =
            let isRec = defaultArg isRec false
            let isCont = defaultArg isCont false
            let trivia = {SynModuleDeclNestedModuleTrivia.EqualsRange = Some range0; ModuleKeyword = Some range0 }
            SynModuleDecl.NestedModule(ci, isRec, decls, isCont, range0, trivia)
            
    type SynBindingReturnInfo with
        static member Create(typeName, ?attributes) =
            let attributes = defaultArg attributes SynAttributes.Empty
            SynBindingReturnInfo.SynBindingReturnInfo(typeName, range0, attributes)
            
    type SynUnionCase with
        member x.HasFields =
            let (SynUnionCase.SynUnionCase(_,_,typ,_,_,_,_)) = x
            match typ with
            | SynUnionCaseKind.Fields fields -> not fields.IsEmpty
            | _ -> false
            
    type SynMatchClause with
        static member Create(pat, whenExp, result) =
            let trivia = {SynMatchClauseTrivia.ArrowRange = Some range0; BarRange = Some range0}
            SynMatchClause.SynMatchClause(pat, whenExp, result, range0, DebugPointAtTarget.No, trivia)

    open DynamicReflection
    type SynExpr with
        static member CreateLambda(pats: SynPat list, body: SynExpr, ?isMember: bool) =
            let isMember = defaultArg isMember false
            let compiler = System.Reflection.Assembly.Load("FSharp.Compiler.Service")
            let syntaxTreeOps = compiler.GetType("FSharp.Compiler.SyntaxTreeOps")
            let synArgNameGenerator = compiler.GetType("FSharp.Compiler.SyntaxTreeOps+SynArgNameGenerator")
            let nameGen = synArgNameGenerator?``.ctor``()
            syntaxTreeOps?mkSynFunMatchLambdas(nameGen, isMember, range0, pats, Some range0, body)
            

            