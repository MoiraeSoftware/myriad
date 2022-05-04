namespace Myriad.Core

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Text.Position
open FSharp.Compiler.Xml
open FSharp.Compiler.SyntaxTrivia

[<AutoOpen>]
module AstExtensions =
    type Ident with
        static member Create text =
            Ident(text, range0)
        static member CreateLong (text: string) =
            text.Split([|'.'|]) |> List.ofArray |> List.map Ident.Create

    type LongIdentWithDots with
        static member Create texts =
            LongIdentWithDots(texts |> List.map Ident.Create, [])
        static member CreateString (text: string) =
            LongIdentWithDots(Ident.CreateLong text, [])
        static member CreateFromLongIdent (longIdent: LongIdent) =
            LongIdentWithDots(longIdent, [])

        member x.AsString =
            let sb = Text.StringBuilder()
            for i in 0 .. x.Lid.Length - 2 do
                sb.Append x.Lid.[i].idText |> ignore
                sb.Append '.' |> ignore
            sb.Append x.Lid.[x.Lid.Length-1].idText |> ignore
            sb.ToString()

    type SynArgPats with
        static member Empty =
            SynArgPats.Pats[]


    type QualifiedNameOfFile with
        static member Create name =
            QualifiedNameOfFile(Ident.Create name)

    type SynMemberFlags with
        static member InstanceMember : SynMemberFlags =
            { IsInstance = true; MemberKind = SynMemberKind.Member; IsDispatchSlot = false; IsOverrideOrExplicitImpl = false; IsFinal = false; Trivia = SynMemberFlagsTrivia.Zero }
        static member StaticMember =
            { SynMemberFlags.InstanceMember with IsInstance = false }

    type SynConst with
        /// Creates a <see href="SynStringKind.Regular">Regular</see> string
        static member CreateString s =
            SynConst.String(s, SynStringKind.Regular, range0)

    type SynExpr with
        static member CreateConst cnst =
            SynExpr.Const(cnst, range0)
        static member CreateConstString s =
            SynExpr.CreateConst (SynConst.CreateString s)
        static member CreateTyped (expr, typ) =
            SynExpr.Typed(expr, typ, range0)
        static member CreateApp (funcExpr, argExpr) =
            SynExpr.App(ExprAtomicFlag.NonAtomic, false, funcExpr, argExpr, range0)
        static member CreateAppInfix (funcExpr, argExpr) =
            SynExpr.App(ExprAtomicFlag.NonAtomic, true, funcExpr, argExpr, range0)
        static member CreateIdent id =
            SynExpr.Ident(id)
        static member CreateIdentString id =
            SynExpr.Ident(Ident.Create id)
        static member CreateLongIdent (isOptional, id, altNameRefCell) =
            SynExpr.LongIdent(isOptional, id, altNameRefCell, range0)
        static member CreateLongIdent id =
            SynExpr.CreateLongIdent(false, id, None)
        static member CreateParen expr =
            SynExpr.Paren(expr, range0, Some range0, range0)
        static member CreateTuple list =
            SynExpr.Tuple(false, list, [], range0)
        static member CreateParenedTuple list =
            SynExpr.CreateTuple list
            |> SynExpr.CreateParen
        static member CreateUnit =
            SynExpr.CreateConst SynConst.Unit
        static member CreateNull =
            SynExpr.Null(range0)
        static member CreateRecord (fields: list<RecordFieldName * option<SynExpr>>) =
            let fields = fields |> List.map (fun (rfn, synExpr) -> SynExprRecordField (rfn, None, synExpr, None))
            SynExpr.Record(None, None, fields, range0)
            
        static member CreateRecordUpdate (copyInfo: SynExpr, fieldUpdates) =
            let blockSep = (range0, None) : BlockSeparator
            let fields = fieldUpdates |> List.map (fun (rfn, synExpr) -> SynExprRecordField(rfn, Some range0, synExpr, Some blockSep))
            let copyInfo = Some (copyInfo, blockSep)
            SynExpr.Record(None, copyInfo, fields, range0)
            
        static member CreateRecordUpdate (copyInfo: SynExpr, fieldUpdates ) =
            let blockSep = (range0, None) : BlockSeparator
            let copyInfo = Some (copyInfo, blockSep)
            SynExpr.Record (None, copyInfo, fieldUpdates, range0)
            
        /// Creates:
        ///
        /// ```
        /// match matchExpr with
        /// | clause1
        /// | clause2
        /// ...
        /// | clauseN
        /// ```
        static member CreateMatch(matchExpr, clauses) =
            SynExpr.Match(range0, DebugPointAtBinding.Yes range0, matchExpr, range0, clauses, range0)
        /// Creates : `instanceAndMethod(args)`
        static member CreateInstanceMethodCall(instanceAndMethod : LongIdentWithDots, args) =
            let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
            SynExpr.CreateApp(valueExpr, args)
        /// Creates : `instanceAndMethod()`
        static member CreateInstanceMethodCall(instanceAndMethod : LongIdentWithDots) =
            SynExpr.CreateInstanceMethodCall(instanceAndMethod, SynExpr.CreateUnit)
        /// Creates : `instanceAndMethod<type1, type2,... type}>(args)`
        static member CreateInstanceMethodCall(instanceAndMethod : LongIdentWithDots, instanceMethodsGenericTypes, args) =
            let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
            let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, instanceMethodsGenericTypes, [], None, range0, range0)
            SynExpr.CreateApp(valueExprWithType, args)
        /// Creates: expr1; expr2; ... exprN
        static member CreateSequential exprs =
            let seqExpr expr1 expr2 = SynExpr.Sequential(DebugPointAtSequential.SuppressBoth, false, expr1, expr2, range0)
            let rec inner exprs state =
                match state, exprs with
                | None, [] -> SynExpr.CreateConst SynConst.Unit
                | Some expr, [] -> expr
                | None, [single] -> single
                | None, [one;two] -> seqExpr one two
                | Some exp, [single] -> seqExpr exp single
                | None, head::shoulders::tail ->
                    seqExpr head shoulders
                    |> Some
                    |> inner tail
                | Some expr, head::tail ->
                    seqExpr expr head
                    |> Some
                    |> inner tail
            inner exprs None


    type SynType with
        static member CreateApp (typ, args, ?isPostfix) =
            SynType.App(typ, None, args, [], None, (defaultArg isPostfix false), range0)
        static member CreateLongIdent id =
            SynType.LongIdent(id)
        static member CreateLongIdent s =
            SynType.CreateLongIdent(LongIdentWithDots.CreateString s)
        static member CreateUnit =
            SynType.CreateLongIdent("unit")
        static member CreateFun (fieldTypeIn, fieldTypeOut) =
            SynType.Fun (fieldTypeIn, fieldTypeOut, range0)

        static member Create(name: string) = SynType.CreateLongIdent name

        static member Option(inner: SynType) =
            SynType.App(
                typeName=SynType.CreateLongIdent "Option",
                typeArgs=[ inner ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member ResizeArray(inner: SynType) =
            SynType.App(
                typeName=SynType.CreateLongIdent "ResizeArray",
                typeArgs=[ inner ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member Set(inner: SynType) =
            SynType.App(
                typeName=SynType.CreateLongIdent "Set",
                typeArgs=[ inner ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member NativePointer(inner: SynType) =
            SynType.App(
                typeName=SynType.CreateLongIdent "nativeptr",
                typeArgs=[ inner ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member Option(inner: string) =
            SynType.App(
                typeName=SynType.CreateLongIdent "Option",
                typeArgs=[ SynType.Create inner ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member Dictionary(key, value) =
            SynType.App(
                typeName=SynType.LongIdent(LongIdentWithDots.Create [ "System"; "Collections"; "Generic"; "Dictionary" ]),
                typeArgs=[ key; value ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member Map(key, value) =
            SynType.App(
                typeName=SynType.CreateLongIdent "Map",
                typeArgs=[ key; value ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member List(inner: SynType) =
            SynType.App(
                typeName=SynType.CreateLongIdent "list",
                typeArgs=[ inner ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member Array(inner: SynType) =
            SynType.App(
                typeName=SynType.CreateLongIdent "array",
                typeArgs=[ inner ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member List(inner: string) =
            SynType.App(
                typeName=SynType.CreateLongIdent "list",
                typeArgs=[ SynType.Create inner ],
                commaRanges = [ ],
                isPostfix = false,
                range=range0,
                greaterRange=None,
                lessRange=None
            )

        static member DateTimeOffset() =
            SynType.LongIdent(LongIdentWithDots.Create [ "System"; "DateTimeOffset" ])

        static member DateTime() =
            SynType.LongIdent(LongIdentWithDots.Create [ "System"; "DateTime" ])

        static member Guid() =
            SynType.LongIdent(LongIdentWithDots.Create [ "System"; "Guid" ])

        static member Int() =
            SynType.Create "int"

        static member UInt() =
            SynType.Create "uint"

        static member Int8() =
            SynType.Create "int8"

        static member UInt8() =
            SynType.Create "uint8"

        static member Int16() =
            SynType.Create "int16"

        static member UInt16() =
            SynType.Create "uint16"

        static member Int64() =
            SynType.Create "int64"

        static member UInt64() =
            SynType.Create "uint64"

        static member String() =
            SynType.Create "string"

        static member Bool() =
            SynType.Create "bool"

        static member Float() =
            SynType.Create "float"

        static member Float32() =
            SynType.Create "float32"

        static member Double() =
            SynType.Create "float"

        static member Decimal() =
            SynType.Create "decimal"

        static member Unit() =
            SynType.Create "unit"

        static member BigInt() =
            SynType.Create "bigint"

        static member Byte() =
            SynType.Create "byte"

        static member Char() =
            SynType.Create "char"

    type SynArgInfo with
        static member Empty =
            SynArgInfo(SynAttributes.Empty, false, None)
        static member CreateId id =
            SynArgInfo(SynAttributes.Empty, false, Some id)
        static member CreateIdString id =
            SynArgInfo.CreateId(Ident.Create id)

    type SynValInfo with
        static member Empty =
            SynValInfo([], SynArgInfo.Empty)

    type SynMemberDefn with
        static member CreateImplicitCtor (ctorArgs) =
            SynMemberDefn.ImplicitCtor(None, SynAttributes.Empty, SynSimplePats.SimplePats(ctorArgs, range0), None, PreXmlDoc.Empty, range0 )
        static member CreateImplicitCtor() =
            SynMemberDefn.CreateImplicitCtor []

        static member CreateInterface(interfaceType, members) =
            SynMemberDefn.Interface(interfaceType, None, members, range0)



    type SynModuleDecl with

        static member CreateOpen id =
            SynModuleDecl.Open(id, range0)
        static member CreateOpen (fullNamespaceOrModuleName: string) =
            SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(Ident.CreateLong fullNamespaceOrModuleName, range0), range0)
        static member CreateHashDirective (directive, values) =
            SynModuleDecl.HashDirective (ParsedHashDirective (directive, values, range0), range0)

        static member CreateAttribute(ident, expr, isProp, ?target) =
                { SynAttribute.TypeName = ident
                  SynAttribute.ArgExpr = expr
                  SynAttribute.Target = target
                  SynAttribute.AppliesToGetterAndSetter = isProp
                  SynAttribute.Range = range0 }
        static member CreateAttributes(attributes) =
            SynModuleDecl.Attributes(attributes, range0)

    type SynAttributeList with
        static member Create(attrs): SynAttributeList =
            {
                Attributes = attrs
                Range = range0
            }

        static member Create(attr): SynAttributeList =
            {
                Attributes = [ attr ]
                Range = range0
            }

        static member Create([<ParamArray>] attrs): SynAttributeList =
            {
                Attributes = List.ofArray attrs
                Range = range0
            }

    type SynAttribute with
        static member Create(name: string) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (SynConst.Unit, range0)
               Range = range0
               Target = None
               TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
            }

        static member Create(name: string, argument: string) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (SynConst.String(argument, SynStringKind.Regular, range0), range0)
               Range = range0
               Target = None
               TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
            }

        static member Create(name: string, argument: bool) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (SynConst.Bool argument, range0)
               Range = range0
               Target = None
               TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
            }

        static member Create(name: string, argument: int) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (SynConst.Int32 argument, range0)
               Range = range0
               Target = None
               TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
            }

        static member Create(name: string, argument: SynConst) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (argument, range0)
               Range = range0
               Target = None
               TypeName = LongIdentWithDots([ Ident.Create name ], [ ])
            }

        static member Create(name: Ident, argument: SynConst) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (argument, range0)
               Range = range0
               Target = None
               TypeName = LongIdentWithDots([ name ], [ ])
            }

        static member Create(name: Ident list, argument: SynConst) : SynAttribute =
            {
               AppliesToGetterAndSetter = false
               ArgExpr = SynExpr.Const (argument, range0)
               Range = range0
               Target = None
               TypeName = LongIdentWithDots(name, [ ])
            }

        static member RequireQualifiedAccess() =
            SynAttribute.Create("RequireQualifiedAccess")

        static member CompiledName(valueArg: string) =
            SynAttribute.Create("CompiledName", valueArg)

    type PreXmlDoc with
        static member Create (lines: string list) =
            let lines = List.toArray lines
            let lineMaxIndex = Array.length lines - 1
            let s = mkPos 0 0
            let e = mkPos lineMaxIndex 0
            let containingRange = mkRange "" s e
            PreXmlDoc.Create(lines, containingRange)

        static member Create (docs: string option) =
            PreXmlDoc.Create [
                if docs.IsSome
                then docs.Value
            ]

        static member Create(docs: string) =
            PreXmlDoc.Create [
                if not (String.IsNullOrWhiteSpace docs)
                then docs
            ]

    type SynSimplePat with
        static member CreateTyped(ident, ``type``) =
            let ssp = SynSimplePat.Id(ident, None, false, false, false, range0)
            SynSimplePat.Typed(ssp, ``type``, range0 )

        static member CreateId(ident, ?altNameRefCell, ?isCompilerGenerated, ?isThis, ?isOptional) =
            SynSimplePat.Id(ident, altNameRefCell,
                            Option.defaultValue false isCompilerGenerated,
                            Option.defaultValue false isThis,
                            Option.defaultValue false isOptional,
                            range0)

    type SynSimplePats with
        static member Create(patterns) =
            SynSimplePats.SimplePats(patterns, range0)