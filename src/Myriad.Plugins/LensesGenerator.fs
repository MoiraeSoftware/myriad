namespace Myriad.Plugins

open System
open System.Reflection
open FSharp.Compiler.Syntax
open FSharp.Compiler.Xml
open Myriad.Core
open Myriad.Core.Ast
open FSharp.Compiler.Text.Range
open FSharp.Compiler.SyntaxTrivia
open FsAst

module DynamicReflection =
    open System
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

module internal CreateLenses =
    let r = range0
    open DynamicReflection
    // Create type that provides access to some types
    type Compiler private () =
      static let compiler = Assembly.Load("FSharp.Compiler.Service")
      static member syntaxTreeOps = compiler.GetType("FSharp.Compiler.SyntaxTreeOps")
      static member synArgNameGenerator = compiler.GetType("FSharp.Compiler.SyntaxTreeOps.SynArgNameGenerator")





    let private wrap lens (wrapperName : Option<string>) =
        match wrapperName with
        | Some name when not (String.IsNullOrWhiteSpace(name)) ->
            let wrapperVar = SynExpr.CreateLongIdent (false, LongIdentWithDots (Ident.CreateLong name, []), None)
            SynExpr.App (ExprAtomicFlag.NonAtomic, false, wrapperVar, SynExpr.CreateParen lens, r)
        | _ -> lens

    let private createLensForRecordField (parent: LongIdent) (wrapperName : Option<string>) (aetherStyle: bool) (field: SynField) =
        let (SynField.SynField(_,_,id,fieldType,_,_,_,_)) = field
        let fieldName = match id with None -> failwith "no field name" | Some f -> f

        let recordType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let pattern =
            let name = LongIdentWithDots.CreateString fieldName.idText
            SynPat.CreateLongIdent(name, [])

        let createLambda (args: SynPat list) (bodyExpr: SynExpr) =
            let parsedData = Some (args, bodyExpr)
                       
            let lambda =
                let compiler = Assembly.Load("FSharp.Compiler.Service")
                let syntaxTreeOps = compiler.GetType("FSharp.Compiler.SyntaxTreeOps")
                let synArgNameGenerator = compiler.GetType("FSharp.Compiler.SyntaxTreeOps+SynArgNameGenerator")
                let nameGen = synArgNameGenerator?``.ctor``()
                let (_a: SynSimplePats list,b: SynExpr) = syntaxTreeOps?PushCurriedPatternsToExpr(nameGen, range0, false, args, None, bodyExpr)
                b
            lambda
            
            

        
        let expr =
            let srcVarName = "x"
            let srcIdent = Ident.Create srcVarName

            // x.Property
            let getBody = LongIdentWithDots.Create [srcVarName; fieldName.idText]
            //let recordArg = SynSimplePat.Typed(SynSimplePat.Id (srcIdent, None, false, false, false, r), recordType, r)
            
            // (x : Record)
            let getPats = [SynPat.CreateTyped(SynPat.CreateNamed(srcIdent), recordType)]
            
            // fun (x : Record) -> x.Property
            let get = createLambda getPats (SynExpr.CreateLongIdent(false, getBody, None))

            let valueIdent = Ident.Create "value"
            let valuePattern = [SynPat.CreateTyped(SynPat.CreateNamed valueIdent, fieldType)]
                //SynSimplePat.Typed(SynSimplePat.Id (valueIdent, None, false, false, false, r), fieldType, r)
            
            // (value : PropertyType)
            //let valueArgPatterns = SynSimplePats.SimplePats ([valuePattern], r)
            let copySrc = SynExpr.CreateLongIdent(false, LongIdentWithDots.Create [srcVarName], None)
            let recordToUpdateName : RecordFieldName = (LongIdentWithDots.CreateString fieldName.idText, true)
            
            // { x with Property = value }
            let recordUpdate = SynExpr.CreateRecordUpdate (copySrc, [(recordToUpdateName, SynExpr.Ident valueIdent |> Some)])

            // (value : PropertyType) -> { x with Property = value }
            let innerLambdaWithValue valueArgs =
                createLambda valueArgs recordUpdate

            let set =
                if aetherStyle then
                    // fun (value : PropertyType) -> (x : Record) -> { x with Property = value }
                    createLambda valuePattern (innerLambdaWithValue getPats)
                    //SynExpr.Lambda (false, true, valueArgPattern, innerLambdaWithValue getPats, None, r, SynExprLambdaTrivia.Zero)
                else
                    // fun (x : Record) (value : PropertyType) -> { x with Property = value }
                    createLambda getPats (innerLambdaWithValue valuePattern)
                    //SynExpr.Lambda (false, true, getPats, innerLambdaWithValue valueArgPattern, None, r, SynExprLambdaTrivia.Zero)

            let tuple = SynExpr.CreateTuple [ SynExpr.CreateParen get; SynExpr.CreateParen set ]

            wrap tuple wrapperName

        SynModuleDecl.CreateLet [SynBinding.Let(pattern = pattern, expr = expr)]

    let private createLensForDU (requiresQualifiedAccess : bool) (parent: LongIdent) (wrapperName : Option<string>) (du : SynUnionCase) =
        let (SynUnionCase.SynUnionCase(_,id,duType,_,_,_,_)) = du
        let (SynField.SynField(_,_,_,fieldType,_,_,_,_)) =
            match duType with
            | SynUnionCaseKind.Fields [singleCase] -> singleCase
            | SynUnionCaseKind.Fields (_ :: _) -> failwith "It is impossible to create a lens for a DU with several cases"
            | _ -> failwithf "Unsupported type"

        let duType =
            LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
            |> SynType.CreateLongIdent

        let getterName = Ident("getter", range0)
        let pattern =
            SynPat.CreateLongIdent(LongIdentWithDots.CreateString "Lens'", [])

        let matchCaseIdentParts =
            if requiresQualifiedAccess then
                (parent |> List.map (fun i -> i.idText)) @ [id.idText]
            else
                [id.idText]

        // The name of the DU case, optionally preceded by the name of the DU itself, if
        // fully qualified access is required
        let fullCaseName = LongIdentWithDots.Create matchCaseIdentParts

        let lensExpression =
            let matchCase =
                let caseVariableName = "x"
                let args = [SynPat.CreateLongIdent (LongIdentWithDots.CreateString caseVariableName, [])]
                let matchCaseIdent = SynPat.CreateLongIdent(fullCaseName, args)

                let rhs = SynExpr.CreateIdent (Ident.Create caseVariableName)
                SynMatchClause.Create(matchCaseIdent, None, rhs)

            let getterArgName = "x"
            let matchOn =
                let ident = LongIdentWithDots.CreateString getterArgName
                SynExpr.CreateLongIdent(false, ident, None)

            let matchExpression = SynExpr.CreateMatch(matchOn, [matchCase])

            let setter =
                let valueIdent = Ident.Create "value"
                let valuePattern = SynSimplePat.Typed(SynSimplePat.Id (valueIdent, None, false, false, false, r), fieldType, r)
                let valueArgPatterns = SynSimplePats.SimplePats ([valuePattern], r)

                let duType =
                    LongIdentWithDots.Create (parent |> List.map (fun i -> i.idText))
                    |> SynType.CreateLongIdent

                let createCase =
                    SynExpr.App (ExprAtomicFlag.NonAtomic, false, SynExpr.LongIdent (false, fullCaseName, None, r), SynExpr.Ident valueIdent, r)
                let innerLambdaWithValue =
                    SynExpr.Lambda (false, true, valueArgPatterns, createCase, None, r, SynExprLambdaTrivia.Zero)
                let recordArg = SynSimplePat.Typed(SynSimplePat.Id (Ident.Create "_", None, false, false, false, r), duType, r)
                let getArgs = SynSimplePats.SimplePats ([recordArg], r)

                SynExpr.Lambda (false, true, getArgs, innerLambdaWithValue, None, r, SynExprLambdaTrivia.Zero)

            let tuple = SynExpr.CreateTuple [ SynExpr.Ident getterName; setter ]

            let getterLet =
                let valData = SynValData.SynValData(None, SynValInfo.Empty, None)
                let synPat = SynPat.Named(Ident.Create "x", false, None, r)
                let synPat = SynPat.Typed(synPat, duType, r)
                let synPat = SynPat.Paren (synPat, r)

                let synPat = SynPat.LongIdent (LongIdentWithDots.CreateString "getter", None, None, None, SynArgPats.Pats [synPat], None, r)

                SynBinding.SynBinding (None, SynBindingKind.Normal, false, false, [], PreXmlDoc.Empty, valData, synPat, None, matchExpression, r, DebugPointAtBinding.NoneAtDo, SynBindingTrivia.Zero)

            let lens = SynExpr.LetOrUse (false, false, [getterLet], tuple, r, { InKeyword = None })

            wrap lens wrapperName

        SynModuleDecl.CreateLet [ SynBinding.Let(pattern = pattern, expr = lensExpression) ]
    let private updateLastItem list updater =
        let folder item state =
            match state with
            | [] -> [updater item]
            | l -> item :: l

        List.foldBack folder list []

    let private (|LongIdentLid|) (ident : LongIdentWithDots) =
        ident.Lid

    let private (|SynTypeAppTypeName|_|) (expr : SynType) =
        match expr with
        | SynType.App (name, _, _, _, _, _, _) -> Some name
        | _ -> None

    let createLensModule (namespaceId: LongIdent) (typeDefn: SynTypeDefn) (attr: SynAttribute) (usePipedSetter: bool) =
        let (SynTypeDefn(synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _range, _trivia)) = typeDefn
        let (SynComponentInfo(_attributes, _typeParams, _constraints, recordId, _doc, _preferPostfix, _access, _range)) = synComponentInfo

        // Append "Lenses" to the module name
        let moduleIdent = updateLastItem recordId (fun i -> Ident.Create $"%s{i.idText}Lenses")

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

        let ident = LongIdentWithDots.Create (namespaceId |> List.map (fun ident -> ident.idText))
        let openTarget = SynOpenDeclTarget.ModuleOrNamespace(ident.Lid, r)
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
