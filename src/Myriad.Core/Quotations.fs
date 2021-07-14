namespace Myriad.Machinery
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Utils
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

type Quotations() =
    static member ToAst(expr : Expr, ?ommitEnclosingType : Type list, ?knownNamespaces : _ Set) =
        let knownNamespaces = defaultArg knownNamespaces Set.empty
        let defaultRange = defaultArg (tryParseRange expr) range0               
    
        let rec exprToAst (expr : Expr) : SynExpr =
            let range = defaultArg (tryParseRange expr) defaultRange
            match expr with
            // parse for constants
            | Value(obj, t) ->
                match obj with
                | :? bool as b -> SynExpr.Const(SynConst.Bool b, range)
                | :? byte as b -> SynExpr.Const(SynConst.Byte b, range)
                | :? sbyte as b -> SynExpr.Const(SynConst.SByte b, range)
                | :? char as c -> SynExpr.Const(SynConst.Char c, range)
                | :? decimal as d -> SynExpr.Const(SynConst.Decimal d, range)
                | :? int16 as i -> SynExpr.Const(SynConst.Int16 i, range)
                | :? int32 as i -> SynExpr.Const(SynConst.Int32 i, range)
                | :? int64 as i -> SynExpr.Const(SynConst.Int64 i, range)
                | :? uint16 as i -> SynExpr.Const(SynConst.UInt16 i, range)
                | :? uint32 as i -> SynExpr.Const(SynConst.UInt32 i, range)
                | :? uint64 as i -> SynExpr.Const(SynConst.UInt64 i, range)
                | :? IntPtr as i -> SynExpr.Const(SynConst.IntPtr(int64 i), range)
                | :? UIntPtr as i -> SynExpr.Const(SynConst.UIntPtr(uint64 i), range)
                | :? single as f -> SynExpr.Const(SynConst.Single f, range)
                | :? double as f -> SynExpr.Const(SynConst.Double f, range)
                | :? string as s -> SynExpr.Const(SynConst.String(s, range), range)
                | :? unit -> SynExpr.Const(SynConst.Unit, range)
                | :? (byte[]) as bs -> SynExpr.Const(SynConst.Bytes(bs, range), range)
                | :? (uint16[]) as is -> SynExpr.Const(SynConst.UInt16s is, range)
                // null literal support
                | null ->
                    let synTy = sysTypeToSynType range t knownNamespaces ommitEnclosingType
                    SynExpr.Typed(SynExpr.Null range, synTy, range)
                | :? Expr as e -> exprToAst e
                | unknown -> failwithf "unknown value: %A" unknown
    
            | Var v ->
                //dependencies.Append v.Type
                let ident = mkIdent range v.Name
                SynExpr.Ident ident
            | DerivedPatterns.Lambdas(vars, (NewRecord _ as body)) ->
                let sp (v: Var) =
                    let id = SynSimplePat.Id(mkIdent range v.Name, None, false ,false ,false, range)
                    let vtype = sysTypeToSynType range v.Type knownNamespaces ommitEnclosingType
                    SynSimplePat.Typed(id, vtype, range)
                                                                      
                let result =
                    List.foldBack (fun v state -> SynExpr.Lambda(false, true, SynSimplePats.SimplePats([sp v], range), state, range)) (vars |> List.concat) (exprToAst body)
                
                SynExpr.Paren(result, range, None, range)
                
            | Lambda(v, body) ->
                //dependencies.Append v.Type
                let id = SynSimplePat.Id(mkIdent range v.Name, None, false ,false ,false, range)
                let vtype = sysTypeToSynType range v.Type knownNamespaces ommitEnclosingType
                let simplePat = SynSimplePat.Typed(id, vtype, range)
                let lambda = SynExpr.Lambda(false, true, SynSimplePats.SimplePats([simplePat], range), exprToAst body, range)
                lambda

    
            | LetRecursive(bindings, body) ->
                let mkBinding (v : Var, bind : Expr) =
                    //dependencies.Append v.Type
                    let vType = sysTypeToSynType range v.Type knownNamespaces ommitEnclosingType
                    let untypedPat = mkVarPat range v
                    let typedPat = SynPat.Typed(untypedPat, vType, range)
                    let synBind = exprToAst bind
                    mkBinding range false typedPat synBind
    
                let bindings = List.map mkBinding bindings
                let synBody = exprToAst body
                SynExpr.LetOrUse(true, false, bindings, synBody, range)
    
            | Let(v, bind, body) ->
                //dependencies.Append v.Type
                let vType = sysTypeToSynType range v.Type knownNamespaces ommitEnclosingType
                let untypedPat = mkVarPat range v
                let typedPat = SynPat.Typed(untypedPat, vType, range)
                let synBind = exprToAst bind
                let synBody = exprToAst body
                let synValData = SynValData.SynValData(None, SynValInfo([[]], SynArgInfo([], false, None)), None)
                let synBinding = SynBinding.Binding(None, SynBindingKind.NormalBinding, false, v.IsMutable, [], PreXmlDoc.Empty, synValData, typedPat, None, synBind, range, SequencePointInfoForBinding.SequencePointAtBinding range)
                SynExpr.LetOrUse(false, false, [synBinding], synBody, range)
    
            | Application(left, right) ->
                let synLeft = exprToAst left
                let synRight = 
                    let astRight = exprToAst right
                    match right with 
                    | (Value _ | Var _) -> astRight
                    | _ -> SynExpr.Paren(astRight, range, None, range)
                SynExpr.App(ExprAtomicFlag.NonAtomic, false, synLeft, synRight, range)
    
            | Sequential(left, right) ->
                let synLeft = exprToAst left
                let synRight = exprToAst right
                SynExpr.Sequential(SequencePointInfoForSeq.SequencePointsAtSeq, true, synLeft, synRight, range)
    
            | TryWith(body, _, _, cv, cb) ->
                let synBody = exprToAst body
                let synPat = mkVarPat range cv
                let synCatch = exprToAst cb
                let synClause = SynMatchClause.Clause(synPat, None, synCatch, range, SequencePointInfoForTarget.SequencePointAtTarget)
                SynExpr.TryWith(synBody, range, [synClause], range, range, SequencePointInfoForTry.SequencePointAtTry range, SequencePointInfoForWith.SequencePointAtWith range)
    
            | TryFinally(body, finalizer) ->
                let synBody = exprToAst body
                let synFinalizer = exprToAst finalizer
                SynExpr.TryFinally(synBody, synFinalizer, range, SequencePointInfoForTry.SequencePointAtTry range, SequencePointInfoForFinally.SequencePointAtFinally range)
    
            | IfThenElse(cond, a, b) ->
                let synCond = exprToAst cond
                let synA = exprToAst a
                let synB = exprToAst b
                SynExpr.IfThenElse(synCond, synA, Some synB, SequencePointInfoForBinding.SequencePointAtBinding range, false, range, range)
    
            | WhileLoop(cond, body) ->
                let synCond = exprToAst cond
                let synBody = exprToAst body
                SynExpr.While(SequencePointAtWhileLoop range, synCond, synBody, range)
    
            // Adapt F# exception constructors to F# idiomatic syntax
            | Coerce(NewObject(ctor, args), t) when t = typeof<exn> && FSharpType.IsExceptionRepresentation ctor.DeclaringType ->
                let exnTy = ctor.DeclaringType
                //dependencies.Append exnTy
                let synExn = sysMemberToSynMember range exnTy knownNamespaces ommitEnclosingType
                match List.map exprToAst args with
                | [] -> synExn
                | [arg] -> SynExpr.App(ExprAtomicFlag.NonAtomic, false, synExn, arg, range)
                | args ->
                    let paren = SynExpr.Tuple(args, [], range)
                    SynExpr.App(ExprAtomicFlag.NonAtomic, false, synExn, paren, range)
    
            | Coerce(e, t) ->
                //dependencies.Append t
                let synExpr = exprToAst e
                let synType = sysTypeToSynType range t knownNamespaces ommitEnclosingType
                let synCoerce = 
                    // Need to manually check for interface. <interface>.IsAssignableFrom(<provided_type :> interface>) does not properly return true
                    if (t.IsInterface && (Array.contains t (e.Type.GetInterfaces()))) || t.IsAssignableFrom e.Type then
                        SynExpr.Upcast(synExpr, synType, range)
                    else
                        SynExpr.Downcast(synExpr, synType, range)
    
                SynExpr.Paren(synCoerce, range, None, range)
    
            | TypeTest(expr, t) ->
                //dependencies.Append t
                let synExpr = exprToAst expr
                let synTy = sysTypeToSynType range t knownNamespaces ommitEnclosingType
                SynExpr.TypeTest(synExpr, synTy, range)
    
            | DefaultValue t ->
                //dependencies.Append t
                let synType = sysTypeToSynType range t knownNamespaces ommitEnclosingType
                let synParam = SynExpr.Const(SynConst.Unit, range)
                SynExpr.New(false, synType, synParam, range)
    
            | NewObject(ctorInfo, args) ->
                //dependencies.Append ctorInfo.DeclaringType
                let synType = sysTypeToSynType range ctorInfo.DeclaringType knownNamespaces ommitEnclosingType
                let synArgs = List.map exprToAst args
                let paramInfo = ctorInfo.GetOptionalParameterInfo()
                let synParam =
                    match List.map2 (mkArgumentBinding range) paramInfo synArgs with
                    | [] -> SynExpr.Const(SynConst.Unit, range)
                    | [a] -> SynExpr.Paren(a, range, None, range)
                    | synParams -> SynExpr.Tuple(synParams, [], range)
    
                SynExpr.New(false, synType, synParam, range)
    
            | NewTuple(args) ->
                let synArgs = List.map exprToAst args
                SynExpr.Tuple(synArgs, [], range)
    
            | NewArray(t, elems) ->
                //dependencies.Append t
                let synTy = sysTypeToSynType range t knownNamespaces ommitEnclosingType
                let synArrayTy = SynType.Array(1, synTy, range)
                let synElems = List.map exprToAst elems
                let synArray = SynExpr.ArrayOrList(true, synElems, range)
                SynExpr.Typed(synArray, synArrayTy, range)
    
            | NewRecord(ty, entries) ->
                //dependencies.Append ty
                let synTy = sysTypeToSynType range ty knownNamespaces ommitEnclosingType
                let fields =
                    match ty with
                    | :? ProvidedRecord as pr -> pr.RecordFields
                    | _ -> FSharpType.GetRecordFields(ty, BindingFlags.NonPublic ||| BindingFlags.Public) |> Array.toList
                let synEntries = List.map exprToAst entries
                let entries = (fields, synEntries) ||> List.map2 (fun f e -> (mkLongIdent range [mkIdent range f.Name], true), Some e, None)
                let synExpr = SynExpr.Record(None, None, entries, range)
                SynExpr.Typed(synExpr, synTy, range)
    
            | NewUnionCase(uci, args) ->
                //dependencies.Append uci.DeclaringType
                let synTy = sysTypeToSynType range uci.DeclaringType knownNamespaces ommitEnclosingType
                let uciCtor = SynExpr.LongIdent(false, mkUciIdent range uci.DeclaringType uci.Name knownNamespaces ommitEnclosingType, None, range)
                let synArgs = List.map exprToAst args
                let ctorExpr =
                    match synArgs with
                    | [] -> uciCtor
                    | [a] ->
                        let synParam = SynExpr.Paren(a, range, None, range)
                        SynExpr.App(ExprAtomicFlag.Atomic, false, uciCtor, synParam, range)
                    | _ ->
                        let synParam = SynExpr.Tuple(synArgs, [], range)
                        SynExpr.App(ExprAtomicFlag.Atomic, false, uciCtor, synParam, range)
                let newUnionCase = SynExpr.Typed(ctorExpr, synTy, range)
                SynExpr.Paren(newUnionCase, range, None, range)
    
            | NewDelegate(t, vars, body) ->
                //dependencies.Append t
                let synType = sysTypeToSynType range t knownNamespaces ommitEnclosingType
                let synBody = exprToAst body
                let rec mkLambda acc (rest : Var list) =
                    match rest with
                    | [] -> acc
                    | v :: tail ->
                        let vType = sysTypeToSynType range v.Type knownNamespaces ommitEnclosingType
                        let spat = SynSimplePat.Id(mkIdent range v.Name, None, false ,false ,false, range)
                        let untypedPat = SynSimplePats.SimplePats([spat], range)
                        let typedPat = SynSimplePats.Typed(untypedPat, vType, range)
                        let synLambda = SynExpr.Lambda(false, false, typedPat, acc, range)
                        mkLambda synLambda tail
    
                let synAbs = mkLambda synBody (List.rev vars)
                SynExpr.New(false, synType, SynExpr.Paren(synAbs, range, None, range), range)
    
            | UnionCaseTest(expr, uci) ->
                //dependencies.Append uci.DeclaringType
                let synExpr = exprToAst expr
                let ctorPat =
                    if isListType uci.DeclaringType then
                        // list pattern match requires special syntax
                        if uci.Name = "Empty" then
                            SynPat.ArrayOrList(false, [], range)
                        else // Cons
                            let uciIdent = mkLongIdent range [mkIdent range "op_ColonColon"]
                            let pats = SynPat.Tuple([SynPat.Wild range ; SynPat.Wild range], range)
                            SynPat.LongIdent(uciIdent, None, None, SynConstructorArgs.Pats [pats], None, range)
                    else
                        let uciIdent = mkUciIdent range uci.DeclaringType uci.Name knownNamespaces ommitEnclosingType
                        let ctorArgs =
                            let fieldLength = 
                                match uci.DeclaringType with
                                | :? ProvidedUnion as _pu ->
                                    //TODO: add logic to providedUnion to figure out the number of fields in the particular unionCase
                                    //At the moment we are only supporting 1 field with proto3 implementation
                                    1
                                | _ -> uci.GetFields().Length
                            if fieldLength = 0
                            then []
                            else [SynPat.Wild range]
                        SynPat.LongIdent(uciIdent, None, None, SynConstructorArgs.Pats ctorArgs, None, range)
    
                let matchClause = SynMatchClause.Clause(ctorPat, None, SynExpr.Const(SynConst.Bool true, range), range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
                let notMatchClause = SynMatchClause.Clause(SynPat.Wild range, None, SynExpr.Const(SynConst.Bool false, range), range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
                SynExpr.Match(SequencePointInfoForBinding.SequencePointAtBinding range, synExpr, [matchClause ; notMatchClause], false, range)
    
            | Call(instance, methodInfo, args) ->
                //dependencies.Append methodInfo.DeclaringType
                let synArgs = List.map exprToAst args
                let paramInfo = methodInfo.GetOptionalParameterInfo()
                let synArgs = List.map2 (mkArgumentBinding range) paramInfo synArgs |> List.toArray
                
                if PrettyNaming.IsMangledOpName methodInfo.Name then
                    let opName = sysMemberToSynMember range methodInfo knownNamespaces ommitEnclosingType
                    SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.App(ExprAtomicFlag.NonAtomic, true, opName, synArgs.[0], range), synArgs.[1], range)
                else
                
                // TODO : need a way to identify F# 'generic values', i.e. typeof<'T>
                // it seems that the only way to do this is by parsing F# assembly signature metadata
                // for now, use a heuristic that happens to hold for FSharp.Core operators
                // but not user-defined values. These are not supported for now.
                let defaultGrouping =
                    if Array.isEmpty synArgs && 
                        (methodInfo.ContainsAttribute<RequiresExplicitTypeArgumentsAttribute> ()
                            || methodInfo.ContainsAttribute<GeneralizableValueAttribute> ()) then []
                    else [synArgs.Length]
    
                let groupings = defaultArg (tryGetCurriedFunctionGroupings methodInfo) defaultGrouping
                let rec foldApp (funcExpr : SynExpr, i : int) (grouping : int) =
                    let args =
                        match grouping with
                        | 0 -> SynExpr.Const(SynConst.Unit, range)
                        | 1 -> SynExpr.Paren(synArgs.[i], range, None, range)
                        | _ -> SynExpr.Paren(SynExpr.Tuple(Array.toList <| synArgs.[i .. i + grouping - 1], [], range), range, None, range)
                    let funcExpr2 = SynExpr.App(ExprAtomicFlag.NonAtomic, false, funcExpr, args, range)
                    funcExpr2, i + grouping
    
                let synMethod = 
                    match instance with
                    | None -> sysMemberToSynMember range methodInfo knownNamespaces ommitEnclosingType
                    | Some inst ->
                        let synInst = exprToAst inst
                        let liwd = mkLongIdent range [mkIdent range methodInfo.Name]
                        SynExpr.DotGet(synInst, range, liwd, range)
    
                let synMethod =
                    if methodInfo.IsGenericMethod then
                        let margs =
                            methodInfo.GetGenericArguments()
                            |> Seq.map (fun t -> match t with
                                                 | TypeHelpers.TypeSuppressed ommitEnclosingType true ->
                                                     let liwd = LongIdentWithDots( [ (getMemberPath range t knownNamespaces ommitEnclosingType) |> List.last], [range])
                                                     SynType.LongIdent liwd 
                                                 | _ -> sysTypeToSynType range t knownNamespaces ommitEnclosingType )
                            |> Seq.toList
                        SynExpr.TypeApp(synMethod, range, margs, [], None, range, range)
                    else
                        synMethod
    
                let callExpr,_ = List.fold foldApp (synMethod, 0) groupings
                callExpr
    
            | TupleGet(tuple, idx) ->
                let synTuple = exprToAst tuple
                let arity = FSharpType.GetTupleElements(tuple.Type).Length
                let ident = mkUniqueIdentifier range
                let synIdent = SynExpr.Ident(ident)
                let patterns = 
                    [ 
                        for _i in 0 .. idx - 1 -> SynPat.Wild range
                        yield SynPat.Named(SynPat.Wild range, ident, false, None, range)
                        for _i in idx + 1 .. arity - 1 -> SynPat.Wild range
                    ]
    
                let synPat = SynPat.Tuple(patterns, range)
                let binding = mkBinding range false synPat synTuple
                SynExpr.LetOrUse(false, false, [binding], synIdent, range)
    
            // pattern matching with union case field binding
            | UnionCasePropertyGet(instance, ucType, ucName, _position) ->
                //dependencies.Append uci.DeclaringType
                let synInstance = exprToAst instance
                let matchArg = mkIdent range "x"
                let fail = 
                    SynExpr.App(ExprAtomicFlag.NonAtomic,false,SynExpr.Ident(mkIdent range "failwith"), SynExpr.Const(SynConst.String("Should never hit",range),range),range)
                let ctorPat =
                    let uciIdent = mkUciIdent range ucType.DeclaringType ucName knownNamespaces ommitEnclosingType
                    let namePatPairs = [(mkIdent range ucName), SynPat.Named(SynPat.Wild range, matchArg, false, None, range)]
                    SynPat.LongIdent(uciIdent, None, None, SynConstructorArgs.NamePatPairs(namePatPairs,range), None, range)
                let matchClause = SynMatchClause.Clause(ctorPat, None, SynExpr.Ident(matchArg), range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
                let notMatchClause = SynMatchClause.Clause(SynPat.Wild range, None, fail, range, SequencePointInfoForTarget.SuppressSequencePointAtTarget)
                SynExpr.Match(SequencePointInfoForBinding.SequencePointAtBinding range, synInstance, [matchClause ; notMatchClause], false, range)
                
            | PropertyGet(instance, propertyInfo, []) ->
                //dependencies.Append propertyInfo.DeclaringType
                match instance with
                | None -> sysMemberToSynMember range propertyInfo knownNamespaces ommitEnclosingType
                | Some inst ->
                    let sysInst = exprToAst inst
                    let liwd = mkLongIdent range [mkIdent range propertyInfo.Name]
                    //match a call, and wrap with parenthesis
                    match inst with
                    | Call _ -> SynExpr.Paren(SynExpr.DotGet(sysInst, range, liwd, range), range, None, range)
                    | _ -> SynExpr.DotGet(sysInst, range, liwd, range)
                    
    
            | PropertyGet(instance, propertyInfo, indexers) ->
                //dependencies.Append propertyInfo.DeclaringType
                let synIndexer =
                    match List.map exprToAst indexers with
                    | [one] -> SynIndexerArg.One(one)
                    | synIdx -> SynIndexerArg.One(SynExpr.Tuple(synIdx, [range], range))
    
                match instance with
                | None -> 
                    let ident = sysMemberToSynMember range propertyInfo.DeclaringType knownNamespaces ommitEnclosingType
                    SynExpr.DotIndexedGet(ident, [synIndexer], range, range)
                | Some inst ->
                    let synInst = exprToAst inst
                    SynExpr.DotIndexedGet(synInst, [synIndexer], range, range)
    
            | PropertySet(instance, propertyInfo, [], value) ->
                //dependencies.Append propertyInfo.DeclaringType
                let synValue = exprToAst value
                match instance with
                | None ->
                    let ident = LongIdentWithDots(getMemberPath range propertyInfo knownNamespaces ommitEnclosingType, [])
                    SynExpr.LongIdentSet(ident, synValue, range)
                | Some inst ->
                    let synInst = exprToAst inst
                    SynExpr.DotSet(synInst, LongIdentWithDots([mkIdent range propertyInfo.Name], []), synValue, range)
    
            | PropertySet(instance, propertyInfo, indexers, value) ->
                //dependencies.Append propertyInfo.DeclaringType
                let synValue = exprToAst value
                let synIndexer = 
                    match List.map exprToAst indexers with
                    | [one] -> SynIndexerArg.One(one)
                    | synIdx -> SynIndexerArg.One(SynExpr.Tuple(synIdx, [range], range))
    
                match instance with
                | None ->
                    let ident = sysMemberToSynMember range propertyInfo.DeclaringType knownNamespaces ommitEnclosingType
                    SynExpr.DotIndexedSet(ident, [synIndexer], synValue, range, range, range)
    
                | Some inst ->
                    let synInst = exprToAst inst
                    SynExpr.DotIndexedSet(synInst, [synIndexer], synValue, range, range, range)
    
            | FieldGet(instance, fieldInfo) ->
                //dependencies.Append fieldInfo.DeclaringType
                match instance with
                | None -> sysMemberToSynMember range fieldInfo knownNamespaces ommitEnclosingType
                | Some inst ->
                    let synInst = exprToAst inst
                    SynExpr.DotGet(synInst, range, mkLongIdent range [mkIdent range (getFSharpName fieldInfo knownNamespaces ommitEnclosingType )], range)
    
            | FieldSet(instance, fieldInfo, value) ->
                //dependencies.Append fieldInfo.DeclaringType
                let synValue = exprToAst value
                match instance with
                | None ->
                    let ident = LongIdentWithDots(getMemberPath range fieldInfo knownNamespaces ommitEnclosingType, [])
                    SynExpr.LongIdentSet(ident, synValue, range)
                | Some inst ->
                    let synInst = exprToAst inst
                    SynExpr.DotSet(synInst, LongIdentWithDots([mkIdent range fieldInfo.Name], []), synValue, range)
            
            | VarSet(v, value) ->
                //dependencies.Append v.Type
                let synValue = exprToAst value
                let synVar = LongIdentWithDots([mkIdent range v.Name], [])
                SynExpr.LongIdentSet(synVar, synValue, range)
    
            | ForIntegerRangeLoop(var, startExpr, endExpr, body) ->
                let varIdent = mkIdent range var.Name
                let synStartExpr = exprToAst startExpr
                let synEndExpr = exprToAst endExpr
                let synBody = exprToAst body
                SynExpr.For(SequencePointAtForLoop range, varIdent, synStartExpr, true, synEndExpr, synBody, range)
    
            | QuoteTyped q -> 
                let synQuote = exprToAst q
                let ident = SynExpr.Ident(mkIdent range "op_Quotation")
                SynExpr.Quote(ident, false, synQuote, false, range)
    
            | AddressOf e ->
                let synExpr = exprToAst e
                SynExpr.AddressOf(true, synExpr, range, range)
                
            | AddressSet(Var v,e') ->
                let synValue = exprToAst e'
                let varIdent = mkLongIdent range [mkIdent range v.Name]
                SynExpr.LongIdentSet(varIdent, synValue, range)
                
            | _ -> notImpl expr
    
        let synExprToLetBinding (expr : SynExpr) =
            let synConsArgs = SynConstructorArgs.Pats [ SynPat.Paren(SynPat.Const(SynConst.Unit, defaultRange), defaultRange)]
            let synPat = SynPat.LongIdent(mkLongIdent defaultRange [mkIdent defaultRange "compiledFunctionName" ], None, None, synConsArgs, None, defaultRange)
            // create a `let func () = () ; expr` binding to force return type compatible with quotation type.
            let seqExpr = SynExpr.Sequential(SequencePointsAtSeq, true, SynExpr.Const(SynConst.Unit, defaultRange), expr, defaultRange)
            let binding = mkBinding defaultRange false synPat seqExpr
            SynModuleDecl.Let(false, [binding], defaultRange)
        
        let moduleDeclsToParsedInput (decls : SynModuleDecl list) =
            let modl = SynModuleOrNamespace([mkIdent defaultRange "compiledFunctionName"], false, true, decls, PreXmlDoc.Empty,[], None, defaultRange)
            let file = ParsedImplFileInput("/QuotationCompiler.fs", false, QualifiedNameOfFile(mkIdent defaultRange "compiledFunctionName"), [],[], [modl], ((* isLastCompiland *) true, (* isExe *) false))
            ParsedInput.ImplFile file
    
        let synExpr = expr |> exprToAst 
        let parsedInput =  [synExpr |> synExprToLetBinding] |> moduleDeclsToParsedInput
        synExpr, parsedInput
