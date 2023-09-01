+++
author = "7sharp9"
date = "2019-04-24"
description = "With Myriad And Falanx"
tags = ["fsharp", "metaprogramming", "quotations", "ast", "typeproviders", "myriad"]
title = "Applied Meta-Programming with Myriad"
aliases = ["/2019/04/24/2019-04-24-applied-metaprogramming-with-myriad"]
+++
This article is all about F# metaprogramming and research that I did which evolved over time, firstly with a project called Falanx that I developed while working for Jet/Walmart, then later the ideas and concepts evolved into Myriad.  <!--more-->

## What is Myriad?

[Myriad][17] is a pre-compilation tool that generates code from code.  It is integrated into the project build cycle via an MSBuild extension.  It is also possible to invoke the tool separately as a [CLI tool][21] but this essay will deal with describing the integration within an MSBuild project as this would be the most common pattern of usage.  
<!--more-->
## Historical basis
Before I dig into the detail its best to get some historical details as the [Myriad][17] project stems from previous meta-programming work that I have been doing over the last year, perhaps also further back through my history of using F# too.  The [Falanx][6] project built the first steps towards [Myriad][17] by taking blocks of functionality from different areas of the F# ecosystem and welding them together to aid in code generation.  Lets go through some of the different way you can utilise meta-programming in F#.    

## Meta-programming 101
F# has a number of meta-programming facilities that I have spoke about before in my previous blog posts:  [Code Quotations][15], [Type Providers][9], [Typed Expressions](https://fsharp.github.io/FSharp.Compiler.Service/typedtree.html), and also the [untyped abstract syntax tree - AST](https://fsharp.github.io/FSharp.Compiler.Service/untypedtree.html).  

### Quotations
[Quotations][15] are backed by reflection and mainly used to transform F# to another language.  They are limited in that they do not represent the whole F# language.  Types and modules are not able to be represented like they are in the F# AST.  They also do not encode F# on a one to one basis in terms of representing F# expressions.  Some elements like discriminated union decomposition, pattern matching and function fixity are not represented in the same form as they occur in F#, details are lost in transformation.  

### Type Providers
[Type Providers][9] use [Quotations][15] to encode method information and use these expression with a skeleton of types produced by some for of schema or input.  [Type Providers][9] can be useful in some limited scenarios.  Building a [Type Provider][9] should not be taken lightly as there can often be a lot of edge cases and debugging before they are production ready.  There's also the fact that [Type Providers][9] can not create any F# constructs like [records][23] or [Discriminated Unions][22].  

### Typed Expressions
Typed expressions are used for whole language or system transformations and is the technique used in [Fable][7] to transpile F# to JavaScript.  You can read more about typed expressions in my [Metamatic blog post][7].  They have no dependency on reflection and do not require any on disk assemblies.  

### Untyped AST
The untyped AST or AST for short is not an area that has been widely used, mainly as working with the AST is quite tricky outside of the compiler and the AST is normally produced by the compiler as a result of the parsing phase.  
- - -
## Falanx Whistle-stop Tour

It is not the scope of this essay to describe [Falanx][6] in detail but reading some of the background on it may provide an insight on how the direction in [Myriad][17] was formed and how some of the technical challenges experienced in [Falanx][6] drove the resulting design decisions.  The following is a quick run through some of the core concepts and issues faced in [Falanx][6].  

[Falanx][6] uses a mixture of [Type Providers][9], _(mainly the [Type Provider SDK][8] not the actual Type Provider invocation mechanism)_ [Quotations][15], and AST manipulation to generate source code.  [Falanx][6] evolved from a minimum viable product where the key input factors were a prototype which could be developed within a few weeks that could take a [protocol buffer][10] schema and generate F# [records][23] and [Discriminated Union][22] types as output.  

This is an example of a `[Protocol Buffer][10] file:
 
```
syntax = "proto3";
message BundleRequest {
  int32 martId = 1;
  string member_id = 2;
}
```

Falanx works by defining an MSBuild extension that references a [Protocol Buffer][10] file and generates another file in response to it containing F# [records][23] and [discriminated unions][22].  

```xml
<ItemGroup>
    <PackageReference Include="Falanx.Sdk" Version="0.4.*" PrivateAssets="All" />
</ItemGroup>

<ProtoFile Include="..\proto\bundle.proto">
    <OutputPath>mycustom.fs</OutputPath>
</ProtoFile>
```
`<ProtoFile Include="..\proto\bundle.proto">` is the input file and `<OutputPath>mycustom.fs</OutputPath>` is the output file.  

The resulting generated source code looks like this:

```
[<CLIMutable>]
type BundleRequest =
    { mutable martId : int option
      mutable memberId : string option }

    static member JsonObjCodec =
        fun martId memberId ->
        { martId = martId
          memberId = memberId }
        <!> Operators.jopt<BundleRequest, Int32> ("martId") (fun x -> x.martId)
        <*> Operators.jopt<BundleRequest, String> ("memberId") (fun x -> x.memberId)

    static member Serialize(m : BundleRequest, buffer : ZeroCopyBuffer) =
        writeOption<Int32>  (writeInt32)  (1) (buffer) (m.martId)
        writeOption<String> (writeString) (2) (buffer) (m.memberId)

    static member Deserialize(buffer : ZeroCopyBuffer) =
        deserialize<BundleRequest> (buffer)

    interface IMessage with
        member x.Serialize(buffer : ZeroCopyBuffer) =
            BundleRequest.Serialize(x, buffer)

        member x.ReadFrom(buffer : ZeroCopyBuffer) =
            let enumerator =
                ZeroCopyBuffer.allFields(buffer).GetEnumerator()
            while enumerator.MoveNext() do
                let current = enumerator.Current
                if current.FieldNum = 2 then x.memberId <- (Some(readString current) )
                else if current.FieldNum = 1 then x.martId <- (Some(readInt32 current) )
                else ()
            enumerator.Dispose()

        member x.SerializedLength() = serializedLength<BundleRequest> (x)
```
The first part of the generated code is a [record][23] definition followed by binary and json serialization methods.  `JsonObjCodec` is used via the [Fleece][11] library, `Serialize` and `Deserialize` are used by the Froto[12] library.  The [records][22] produced support both binary and json serialization via the [Froto][12] and [Fleece][11] libraries respectively.  [Fleece][11] ss interesting in that the generated code uses applicative style and only requires a codec property `JsonObjCodec` to serialize and deserialize to json:  

```
//serialize
let bundleRequest = {name = Some 42; memberId = Some "172c" }
printfn "%s" (string (toJson bundleRequest))

//deserialize
let bundleRequest2 = parseJson<BundleRequest> """{"martId": "54", "memberId": "172d"}"""
```

#### Parsing/AST

Part of the technical challenge of [Falanx][6] was the time pressure to build a minimum viable product in a short amount of time.  [Type Providers][9] were out of the question as one of the main requirements were to produce F# [records][23] and [Discriminated Unions][22].

The [Froto][12] library already had a working [Type Provider][9] but the resulting output code was not F# [records][23] and [Discriminated Unions[22] as [Type Providers][9] only support basic CLR types no F# specific types are supported.  Another issue was that [Protocol Buffers][10] version 2 was the only version supported, so we decided to use the parser from [Froto][12] and make a PR to update it to support Protocol Buffers 3 syntax.  

#### Quotations
We now have the Protocol Buffer 3 file represented as an abstract syntax tree.  In [Froto][12] there also exist [quotations][15] that were used for the Provided Type methods that were used in the [Froto][12] [Type Provider][9], although we were not using the [Type Provider[9] in [Froto][12], we could reuse some of these [quotations][15] and adapt them for our needs.  Extra [quotations][15] were also created to form the `JsonObjCodec` property from the code above.  

#### Quotations -> AST
The next part was to take the [quotations][15] representing `Serialize`, `Deserialize` and `JsonObjCodec` and convert them to source code.  Both quotations and the F# AST represent similar but not quite the same things:  A collection of nodes that represent the abstract notion of code.  [Quotations][15] do not map fully into the F# AST as they only represent a subset of the AST, types for example are not present in [quotations][15], neither can quotation map functions like for like into F# AST nodes as [quotations][15] lack detail compared the the F# AST, such as fixity information, pattern matching and decomposition of Discriminated Unions are all altered in the quotation of literals or composing of Quotation Expressions.  

Theres an interesting library called [Quotation Compiler][13] by [Eirik Tsarpalis](https://github.com/eiriktsarpalis), inside this library there a piece of code which uses an entry point into the F# compiler that allows you to compile an AST to a dll rather than using a source file.  I remember looking through this previously and wondered if something similar could be used.  I also found that there was a function that transformed [quotations][15] to fragments of an AST too.  I found that this could be adapted to what I needed with some simple changes.  Unfortunately it was not possible to reference this library directly as in the end I needed to heavily modify it to work with the [quotations][15] that referenced Provided types due to reflection issues, but it did form the basis for the bulk of the solution.  The reflection issue it to do with the way [Type Providers][9] are represented.  Each type produced in a generative type provider is backed by a subtype of one of the reflection base types: 

Type|Base
----|----
ProvidedTypeSymbol | TypeDelegator
ProvidedSymbolMethod | MethodInfo
ProvidedStaticParameter | ParameterInfo
ProvidedParameter | ParameterInfo
ProvidedConstructor | ConstructorInfo
ProvidedMethod | MethodInfo
ProvidedProperty | PropertyInfo
ProvidedEvent | EventInfo
ProvidedField | FieldInfo
ProvidedTypeDefinition | TypeDelegator
MethodSymbol2 | MethodInfo
ConstructorSymbol | ConstructorInfo
MethodSymbol | MethodInfo
PropertySymbol | PropertyInfo
EventSymbol | EventInfo
FieldSymbol | FieldInfo
Type Symbol | TypeDelegator
TargetGenericParam | TypeDelegator
TargetTypeDefinition | TypeDelegator
TargetModule | Module
TargetAssembly | Assembly
ProvidedAssembly | Assembly

These can start to become an issue when you start to use reflection on the Provided Types as the [Type Provider SDK][8] only implements the minimal overloads from the base types so you can quickly encounter not implemented exceptions quite easily.  An example of this is if you tried to access the `ReflectedType` property of a `ProvidedProperty`

```
    let notRequired this opname item =
        let msg = sprintf "The operation '%s' on item '%s' should not be called on provided type, member or parameter of type '%O'. Stack trace:\n%s" opname item (this.GetType()) Environment.StackTrace
        Debug.Assert (false, msg)
        raise (NotSupportedException msg)

    type ProvidedProperty(...) =
        inherit PropertyInfo()
        ...
        override this.ReflectedType = notRequired this "ReflectedType" propertyName  
```
This ended up requiring some creative workarounds and reverse engineering of the reflection functionality in FSharp.Core which was quite time consuming.  An example of this when converting a `NewRecord` quotation expression inot an aST fragment:

```
    match expr with
    | NewRecord(ty, entries) ->
        let synTy = sysTypeToSynType range ty knownNamespaces ommitEnclosingType
        let fields =
            match ty with
            | :? ProvidedRecord as pr -> pr.RecordFields
            | _ -> FSharpType.GetRecordFields(ty, BindingFlags.NonPublic ||| BindingFlags.Public) |> Array.toList
        let synEntries = List.map exprToAst entries
        let entries = (fields, synEntries) ||> List.map2 (fun f e -> (mkLongIdent range [mkIdent range f.Name], true), Some e, None)
        let synExpr = SynExpr.Record(None, None, entries, range)
        SynExpr.Typed(synExpr, synTy, range)
```
If you look at `match ty with` you can see that theres a special pattern match when the `ty` is a `ProvidedRecord` which calls `pr.RecordFields` instead of `FSharpType.GetRecordFields`.   This is because `GetRecordFields` accesses reflection data that is not implemented fully in the [Type Provider SDK][8] this is due to custom attributes that FSharp.Core expect to be present.  

Another aspect of this was transforming ProvidedTypes into AST nodes.  As well as the built in types in the [Type Provider SDK][8] I added several new types namely a `ProvidedUnion`, `ProvidedUnionCase` and a `ProvidedRecord`.  These custom types along with normal Provided Types were mapped into AST fragments along with the quotations that had been transformed into AST nodes.  The combination of all these elements were used to create a complete AST which comprised of Modules, Records with member functions and Discriminated Unions with member functions.  

#### AST -> Source code
We did not need the output to be a dll but actual source code as this was another of the main requirements, to do this I used the [Fantomas][4] library which allows you to use an AST as input and get back the source code the AST represents.  

#### Summary of Challenges with Falanx

Theres were quite a few challenges with Falanx, mainly around the usage and consumption of [quotations][15].  In other languages quoting and unquoting is a first class part off the language, however, this is not so with F# so there are lots of pitfalls whilst working with [quotations][15].

*  [Quotations][15] are really difficult to work with when you need to compose complex functions, even more so if you mix in [Statically Resolved Type Parameters][14] and reflection.  

*  [Quotations][15] loose detail when you use quotation literals _(sections of code enclosed by the `<@`/`@>` and `<@@`/`@@>` operators_).  For example pattern matching gets transformed into if else blocks, fixity information is lost so you don't know if an operator was called with infix or prefix notation etc.  _(I raised some of these issues as F# language suggestions: [Add union match patterns to Expr](https://github.com/fsharp/fslang-suggestions/issues/681), [Add support for recording fixity in quoted literals](https://github.com/fsharp/fslang-suggestions/issues/680))_

*  [Quotations][15] do not represent the full range of expressions possible with F#.  Types and Modules are not representable, Discriminated Union decomposition has no form of Quotation.

*  The transformed [quotations][15] are not always elegant idiomatic F# code, mainly for the reason above and the fact that pattern matching is erased etc.

*  Splicing types into [quotations][15] can be really tricky especially if you are using a library that uses a lot of generic parameters such as Fleece.  Type inference in a library like Fleece makes it really nice to use with custom [applicative operators](https://github.com/mausch/Fleece#codec) but defining generic functions with 5 or more generic types in the type signature is not fun at all.  Theres is an [approved language proposal](https://github.com/fsharp/fslang-suggestions/issues/670) to add the splicing of types with the `~` operator which would help with this somewhat.  

*  Working with Provided Types from the [Type Provider SDK][8] can be really challenging with [quotations][15] as [quotations][15] are backed by reflection and Provided Types have bare minimum reflection implemented.   There are often times where composing Quotations will fail at runtime due to a missing reflection implementation.  This sometimes required a patch to either the reflection implementation in the [Type Provider SDK][8] or a custom reflection implementation to extract information from the Provided Types.  

All in all it was quite an intense development processes with lots and lots of debugging and digging through FSharp.Core code and coming up with creative solutions _(Hacks!)_ to reflection issues.  I could probably write an entire article all about the pitfalls, tricks and tips of working with quotations without shooting your self in the foot with a blunderbuss.
  
- - -
# Myriad Technical Overview
#### _Ok enough with Falanx what is Myriad Again?_

[Myriad][17] is similar to the [ppx_deriving][1] extension for OCaml except that it's not as integrated as OCaml.  Although F# compiler integration is desirable for a tool like this there is the question of whether or not this would be in scope for the future direction of the F# language.  There is also the latency factor of writing and extension for the F# compiler and waiting for a release cycle so that it becomes generally available for everyone to use.  By leveraging MSBuild its possible to come close to some of the capabilities and functionality of [ppx_deriving][1].  At the very least it progresses the notion of what more advanced macro like capabilities of F# could look like.

[ppx_deriving][1] works by allowing a type to extended by an arbitrary function, there are various built in plugins such as `show`, `eq`, `ord`, `enum`, `iter`, `map`, `fold`, `make`, `yojson` and `protobuf`.  

Another well known plugin is [ppx_fields_conv][2]:
>Generation of accessor and iteration functions for OCaml records.
ppx_fields_conv is a ppx rewriter that can be used to define first class values representing record fields, and additional routines, to get and set record fields, iterate and fold over all fields of a record and create new record values.  

This was the basis for the idea of Myriad.  We will not be taking the full capability of [ppx_fields_conv][2] only a subset to show the potential of this approach.  More specifically we will be creating field accessor functions and a create function for each record in the input.  

## Usage And Demo

### Input code

[Myriad][17] works with an input file as the basis for code generation, specifically records within the input file are used in the code generation phase.

```
namespace Example

type Test1 = { one: int; two: string; three: float; four: float32 }
type Test2 = { one: Test1; two: string }
```

[Myriad][17] is invoked via the following addition to an F# project file:

```
<Compile Include="Generated.fs" > <!--1-->
    <MyriadFile>..\..\src\Example\Library.fs</MyriadFile> <!--2-->
    <MyriadNameSpace>Test</MyriadNameSpace> <!--3-->
</Compile>
```
1. The `<Compile Include="..."` element is used to specify the output name and also to make sure that the generated file is used during compilation.
2. `<MyriadFile>...` is used to choose the file as input to the [Myriad][17] code generation.  
3. `<MyriadNameSpace>...` is used to specify a namespace to use for the generated code.  If this is omitted then the `RootNamespace` from the project file is used.  

### Output code

```
//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------
namespace rec Test

module Test1 =
    open Example

    let one (x : Test1) = x.one
    let two (x : Test1) = x.two
    let three (x : Test1) = x.three
    let four (x : Test1) = x.four

    let create (one : int) (two : string) (three : float) (four : float32) : Test1 =
        { one = one
          two = two
          three = three
          four = four }

module Test2 =
    open Example

    let one (x : Test2) = x.one
    let two (x : Test2) = x.two

    let create (one : Test1) (two : string) : Test2 =
        { one = one
          two = two }
```

The technical aspects of this project derive from four main areas: parsing, AST construction, code output and build integration.

## Parsing

The first step is gathering information from the input file which is easily done using [FSharp.Compiler.Services][3].  Its easy enough to extract the AST from a piece of code using something such as:

```
let filename = "test.fs"
let fileText = File.ReadAllText filename
let checker = FSharpChecker.Create()
let projOptions, _ = checker.GetProjectOptionsFromScript(filename, fileText) |> Async.RunSynchronously

let ast =
    let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions(projOptions)
    let parseFileResults = checker.ParseFile(file, input, parsingOptions) |> Async.RunSynchronously
    match parseFileResults.ParseTree with
    | Some tree -> tree
    | None -> failwith "Something went wrong during parsing!"
```

A `checker` is created and `projectOptions` are created using the `filename` and `fileText` as input.
Now the checker can be used to extract an ast by first creating `parsingOptions` and passing them to the `checker.ParseFile`, this function returns an option which we pattern match, throwing an exception if there is no Ast present.  

Now that we have the Ast we can use more pattern matching to try and find Ast nodes that we are interested in.  This can be done using a small section of dense pattern matching and decomposition:

```
match ast with
| ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,modules,_)) ->
    for SynModuleOrNamespace(namespaceIdent,_,_,moduleDecls,_,_,_,_) in modules do
        for moduleDecl in moduleDecls do
            match moduleDecl with
            | SynModuleDecl.Types(types,_) ->
                for TypeDefn(ComponentInfo(_,_,_,recordIdent,_,_,_,_), typeDefRepr,_,_) in types do
                    match typeDefRepr with
                    | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_,fields,_),_) ->
                        yield (namespaceIdent,recordIdent,fields)
//...
```
In this snippet you can see that we traverse the AST first decomposing `ParsedInput.ImplFile`, we do this so that we don't have to extract further information in another match such as:

```
match ast with
| ParsedInput.ImplFile(pu) ->
    match pu with ParsedImplFileInput(_,_,_,_,_,modules,_)
```

Now we can loop through the modules/namespaces.  We then drill deeper until we find type definitions within the module.  Once we have found a type definition we can then match on a record node which is a `SynTypeDefnSimpleRepr.Record` type.  Once we have found a record we can extract and yield and parameters that we need for the generator.  In this instance all we need is the parent namespace which we can find from `SynModuleOrNamespace(namespaceIdent,_,_,_,_,_,_,_)`, the record identifier which we find in the type definitions `ComponentInfo`:  `TypeDefn(ComponentInfo(_,_,_,recordIdent,_,_,_,_),_,_,_)`.  The final parameter we need is the fields of the record which are in the record definition itself: `SynTypeDefnSimpleRepr.Record(_,fields,_)`.  In this section you can see that we heavily used pattern matching and discriminated union decomposition, which are ideal for this particular task.  

## Ast Construction  
Now that we have the information we need we can now go about constructing the modules and functions that we want to generate.  

To help with AST construction we use a library called [FSAst][16].  I also used this in Falanx but thought I would explain it more detail here.  

The principle behind [FsAst][16] is that it wraps common AST nodes with record types that have default values, this allows us to use record update syntax.  Most of the AST nodes have a lot of parameters and it can be annoying and cumbersome to construct them.  

For example this is an example of `Let x = 42`:

Let is a node of the type `SynModuleDecl`, here is the definition:  

```
SynModuleDecl =
| ModuleAbbrev of ident: Ident * longId: LongIdent * range: range
| NestedModule of SynComponentInfo * isRecursive: bool * SynModuleDecls * bool * range: range
| Let of isRecursive: bool * SynBinding list * range: range
| DoExpr of SequencePointInfoForBinding * SynExpr * range: range
| Types of SynTypeDefn list * range: range
| Exception of SynExceptionDefn * range: range
| Open of longDotId: LongIdentWithDots * range: range
| Attributes of SynAttributes * range: range
| HashDirective of ParsedHashDirective * range: range
| NamespaceFragment of SynModuleOrNamespace
```

The single Let binding `Let x = 42` looks like this:
```
Let
   (false,
    [Binding
       (None,NormalBinding,false,false,[],
        PreXmlDoc ((2,5),Microsoft.FSharp.Compiler.Ast+XmlDocCollector),
        SynValData (None,SynValInfo ([],SynArgInfo ([],false,None)),None),
        Named
          (Wild tmp.fsx (2,4--2,5) IsSynthetic=false,x,false,None,
           tmp.fsx (2,4--2,5) IsSynthetic=false),None,
        Const (Int32 42,tmp.fsx (2,8--2,10) IsSynthetic=false),
        tmp.fsx (2,4--2,5) IsSynthetic=false,
        SequencePointAtBinding tmp.fsx (2,0--2,10) IsSynthetic=false)],
    tmp.fsx (2,0--2,10) IsSynthetic=false)
```

On its own it would be defined as `SynModuleDecl.Let(false, bindings, range)`, which would require the construction of a list of Bindings, a Binding is defined like this:

```
SynBinding =
| Binding of
    accessibility: SynAccess option *
    kind: SynBindingKind *
    mustInline: bool *
    isMutable: bool *
    attrs: SynAttributes *
    xmlDoc: PreXmlDoc *
    valData: SynValData *
    headPat: SynPat *
    returnInfo: SynBindingReturnInfo option *
    expr: SynExpr  *
    range: range *
    seqPoint: SequencePointInfoForBinding
```

```
SynBinding.Binding(
    None,
    NormalBinding,
    false,
    false,
    [],
    PreXmlDoc(range.zero, XmlDocCollector()),
    SynValData(None, SynValInfo([], SynArgInfo ([], false, None)), None),
    Named(Wild, "tmp.fsx, range.zero, IsSynthetic=false, "x", false, None, "tmp.fsx", range.zero, IsSynthetic=false),
    None,
    Const (Int32, 42,"tmp.fsx", range.zero, IsSynthetic=false),
    "tmp.fsx", range.zero, IsSynthetic=false,
    SequencePointAtBinding("tmp.fsx", range.zero, IsSynthetic=false)
```

You can see that defining these nodes can get complicated really quickly.  With [FsAst][16] we can define the above Let AST fragment like this:  

```
SynModuleDecl.CreateLet(
    { SynBindingRcd.Let with
        Pattern = SynPatRcd.CreateNamed(Ident.Create "x", SynPatRcd.CreateWild)
        Expr = SynExpr.CreateConst(SynConst.Int32 42)
    } )
```
This makes constructing aST fragments a lot easier!

Here is a snippet of code from [Myriad][17] which create a field mapping for a record:
```
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
```

Let me run through the sections that make up a Let binding, lets use this an an example:
```
let one (x : Test1) = x.one
```

#### Pattern
`Pattern` is the bindings name, in this case its: `one (x : Test1)`
We create a `LongIdentifier` from the fieldNames.idText as the name of the Let binding will be the same as the field name, we then make an argument using `"x"` as the `varName`

#### Expr
`Expr` is the expression that you are binding to the name, so this is: `x.one`, which is essentially just a `LongIdentWithDots` of `varName` `.` `fieldName`.  

#### ValData
`ValData` Is information about the argument names and other metadata for a parameter for a member or function.  Such as if the parameter optional or any attributes applied to the parameter.  In this instance we just add the identifier for the argument.  

Using FsAst makes things a lot easier to build F# AST's with code, but there is still a lot of improvements that could be made with `Ident` creation and other areas, there are possibly lots of functions withing the compiler that could be exposed to help with this too.   

## Code Output  
Actual code generation can be done using the F# formatter tool [Fantomas][4].  [Fantomas][4] has an API call that accepts an AST and formats the code that it represents.  All we have to do is make a call to that API to get back formatted source code and append it onto a header:

```
let sourceCode = Fantomas.CodeFormatter.FormatAST(ast, filename, None, fantomasConfig)
```

This can now have a header inserted and be written to a file:  
```
let code =
    [   "//------------------------------------------------------------------------------"
        "//        This code was generated by myriad."
        "//        Changes to this file will be lost when the code is regenerated."
        "//------------------------------------------------------------------------------"
        formattedCode ]
    |> String.concat Environment.NewLine

File.WriteAllText(outputFile, code)
```

## MSBuild integration  
Wrapping the parsing and generation of code in a manner that is easy to use is done via an MSBuild extension, this gives a close approximation to the use of [ppx_deriving][1] and its role within the OCaml ecosystem.  

This is achieved by adding two child attributes to the `Compile` MSBuild element as follows:

```
<Compile Include="Generated.fs">
    <MyriadFile>..\..\src\Example\Library.fs</MyriadFile>
    <MyriadNameSpace>Test</MyriadNameSpace>
</Compile>
```
The `<Compile Include="Generated.fs" >` element is used to specify the output name and also to make sure that the generated file is used during compilation.  

`<MyriadFile>..\..\src\Example\Library.fs</MyriadFile>` is used to choose the file as input to the code generation.  

`<MyriadNameSpace>Test</MyriadNameSpace>` is used to specify a namespace to use for the generated code.  If this is omitted then `RootNamespace` is used.  

### _MyriadSdkFilesList Target

In order for the integration to occur `MyriadFile` and `MyriadNameSpace` have to be processed by the MSBuild extension to form a list of `Compile` element extensions that we can then use to form as an input to a CLI/Command line tool.  This is done in the `_MyriadSdkFilesList` Target, this first part is shown below:   
```
<Target Name="_MyriadSdkFilesList" BeforeTargets="MyriadSdkGenerateInputCache">
    <ItemGroup>
        <MyriadSource Include="%(Compile.MyriadFile)" Condition=" '%(Compile.MyriadFile)' != '' ">
            <OutputPath>$([System.IO.Path]::GetFullPath('%(Compile.FullPath)'))</OutputPath>
            <Namespace Condition=" '%(Compile.MyriadNamespace)' != '' " >%(Compile.MyriadNamespace)</Namespace>
            <Namespace Condition=" '%(Compile.MyriadNamespace)' == '' " >$(RootNamespace)</Namespace>
        </MyriadSource>
    </ItemGroup>

    <ItemGroup>
        <MyriadCodegen Include="%(MyriadSource.FullPath)">
            <OutputPath Condition=" '%(MyriadSource.OutputPath)' != '' ">$([System.IO.Path]::GetFullPath('%(MyriadSource.OutputPath)'))</OutputPath>
            <OutputPath Condition=" '%(MyriadSource.OutputPath)' == '' ">%(MyriadSource.FullPath).fs</OutputPath>
            <Namespace>%(MyriadSource.Namespace)</Namespace>
        </MyriadCodegen>
    </ItemGroup>

    <PropertyGroup>
        <_MyriadSdkCodeGenInputCache>$(IntermediateOutputPath)$(MSBuildProjectFile).FalanxSdkCodeGenInputs.cache</_MyriadSdkCodeGenInputCache>
    </PropertyGroup>
</Target>
```
We first gather a list of files for [Myriad][17] to process.  We do this by creating an `ItemGroup` which is a list of `MyriadSource` elements, only `Compile` elements that have a `MyriadFile` node are processed, this is done via the `Condition` attribute: `Condition=" '%(Compile.MyriadFile)' != ''`.  The `MyriadSource` element is formed from three pieces of information.  

The `Include` attribute is the `MyriadFile` element we include in the MSBuild file.  
The `OutputPath` element is full path for the `Compile` elements `Include` attribute, this is also known as `Identity`
The `Namespace` element is either the `%(Compile.MyriadNamespace)` if it is present or the `$(RootNamespace)` if it is not.  
- - -
Now that we have created an `ItemGroup` containing `MyriadSource` elements we can refine this a little, you could fold these changes into the `MyriadSource` `ItemGroup` but it is easier to create two `ItemGroup` elements.

We create a new `ItemGroup` called `MyriadCodegen` which references `MyriadSource` for its `Include` attribute.  There are also `Condition` attributes to check the `OutputPath` is not empty and also another to ensure that if it is empty to just set it to the input file.   This would mean that the input file itself would be processed and changed rather than being written to another file.  

### MyriadSdkGenerateCode Target

The final step is to invoke the CLI tool with all the information we have gathered in the `MyriadSdkGenerateCode` target:
```
<PropertyGroup>
    <MyriadSdkGenerateCodeDependsOn>$(MyriadSdkGenerateCodeDependsOn);ResolveReferences;MyriadSdkGenerateInputCache</MyriadSdkGenerateCodeDependsOn>
</PropertyGroup>

<Target Name="MyriadSdkGenerateCode"
        DependsOnTargets="$(MyriadSdkGenerateCodeDependsOn)" 
        BeforeTargets="CoreCompile"
        Condition=" '$(DesignTimeBuild)' != 'true' "
        Inputs="@(MyriadCodegen);$(_MyriadSdkCodeGenInputCache);$(MyriadSdk_Generator_Exe)"
        Outputs="%(MyriadCodegen.OutputPath)">

    <PropertyGroup>
        <_MyriadSdk_InputFileName>%(MyriadCodegen.Identity)</_MyriadSdk_InputFileName>
        <_MyriadSdk_OutputFileName>%(MyriadCodegen.OutputPath)</_MyriadSdk_OutputFileName>
        <_MyriadSdk_Namespace>%(MyriadCodegen.Namespace)</_MyriadSdk_Namespace>
    </PropertyGroup>

    <ItemGroup>
        <MyriadSdk_Args Include='--inputfile "$(_MyriadSdk_InputFileName)"' />
        <MyriadSdk_Args Include='--outputfile "$(_MyriadSdk_OutputFileName)"' />
        <MyriadSdk_Args Include='--namespace "$(_MyriadSdk_Namespace)"' />
    </ItemGroup>

    <!-- Use dotnet to execute the process. -->
    <Exec Command="$(MyriadSdk_Generator_ExeHost)&quot;$(MyriadSdk_Generator_Exe)&quot; @(MyriadSdk_Args -> '%(Identity)', ' ')" />
</Target>
```

The `DependsOnTargets` attribute is used to ensure that anything contained in the `MyriadSdkGenerateCodeDependsOn` element is ran before this Target.

Within the `MyriadSdkGenerateCode` `Target` element there are `Inputs` and `Outputs` attributes, these are used to determine when [Myriad][17] needs to run.  An item is considered up-to-date if its output file is the same age or newer than its input file or files.  

We create a `PropertyGroup` to contain the command line parameters `_MyriadSdk_InputFileName`, `_MyriadSdk_OutputFileName` and `_MyriadSdk_Namespace` using the corresponding elements from the  `_MyriadSdkFilesList` Targets ItemGroup `MyriadCodegen`.

we now create an `ItemGroup` which has within it three `MyriadSdk_Args` elements that we need to invoke the code generator with.

Finally we execute the code generator with `Exec` invoking the CLI too, with the parameters from `MyriadSdk_Args` `Include` attribute via the MSBuild function `@(MyriadSdk_Args -> '%(Identity)', ' ')`

One thing that was not discussed in this section was the `_MyriadSdkCodeGenInputCache` that was referenced in both targets above:
```
<Target Name="MyriadSdkGenerateInputCache" DependsOnTargets="ResolveAssemblyReferences;_MyriadSdkFilesList" BeforeTargets="MyriadSdkGenerateCode">

    <ItemGroup>
        <MyriadSdk_CodeGenInputs Include="@(MyriadCodegen);@(ReferencePath);$(MyriadSdk_Generator_Exe)" />
    </ItemGroup>

    <Hash ItemsToHash="@(MyriadSdk_CodeGenInputs)">
        <Output TaskParameter="HashResult" PropertyName="MyriadSdk_UpdatedInputCacheContents" />
    </Hash>

    <WriteLinesToFile Overwrite="true" File="$(_MyriadSdkCodeGenInputCache)" Lines="$(MyriadSdk_UpdatedInputCacheContents)" WriteOnlyWhenDifferent="True" />

</Target>
```

This target generates a hash using `@(MyriadCodegen);@(ReferencePath);$(MyriadSdk_Generator_Exe)` as an input.  If any of those changes then a different hash will be written to the output file via `<WriteLinesToFile Overwrite="true" File="$(_MyriadSdkCodeGenInputCache)"`.  This captures the total set of all inputs to the code generator.  This is based on the _GenerateCompileDependencyCache target from the .NET project system, which was used as a reference.  You can find this in the .Net project system [source][5]. 

# Summary

Wow this was a lot longer than I thought it would be, I hope I didn't ramble too much, its quite a large area once you try to dissect the details.  This are probably multiple essays I could write on each topic.  

I think the key points I summed up at the end of [Falanx][6] were that quotations were not easy to work with, and thats certainly true if you try pushing them right to their limits.  I think [Type Providers][9] and applicative programming with many generic parameters certainly does that!

Falanx was a great project that showed that even out of the box libraries can be used to compose quite a complex project.  Building up a quotations from both literals and expressions takes a lot of time and often you have to run a lot of debug cycles to get things right.  [Myriad][17] built on that by providing a simpler solution of composing the AST directly.  I think there is some scope for a middle ground between the two where a quotation literals could be used as shortcut to provided an AST snippet much in the same way you can do this with [Haxe][19] [Expression Reification][20].  Its sort of similar to the way you can use quotations to extract a `MethodInfo`.  This is used quote a bit in [Falanx.Machinery](https://github.com/7sharp9/falanx/blob/master/src/Falanx.Machinery/Expr.fs#L199):
```
<@@ writeEmbedded x x x @@>
|> Expr.methodof
|> Expr.callStatic [position; buffer; value]
```
I think some creative uses of moving to and from quotations and the AST would allow a lot more flexibility in meta-programming.  

You can find the repositories for both [Myriad][17] and [Falanx][6] on github as soon as Ive made this post public, they are both also available on Nuget too: [Falanx.Sdk](https://www.nuget.org/packages/Falanx.Sdk/)/[Myriad.Sdk](https://www.nuget.org/packages/Myriad.Sdk/)

I hope you enjoyed this post on applied meta-programming and hope to expand on this subject and many others on my [YouTube channel][18].  

Until next time!  

# References
[Type-driven code generation for OCaml - ppx_deriving][1]  
[Generation of accessor and iteration functions for OCaml records][2]  
[Compiler Services: Processing untyped syntax tree][3]  
[Fantomas F# source code formatter][4]  
[.NET project System:- _GenerateCompileDependencyCache][5]  
[Falanx protobuf Code Generation][6]  
[Meta-matic][7]  
[Type Provider SDK][8]  
[Type Providers][9]  
[Protocol Buffers][10]  
[Fleece][11]  
[Froto][12]  
[Quotation Compiler][13]  
[Statically Resolved Type Parameters][14]  
[Quotations][15]  
[FsAst][16]  
[Myriad][17]  
[My YouTube Channel][18]  
[Haxe][19]  
[Expression Reification][20]  
[CLI Tool][21]  
[Discriminated Unions][22]  
[Records][23]


[1]: https://github.com/ocaml-ppx/ppx_deriving
[2]: https://github.com/janestreet/ppx_fields_conv
[3]: https://fsharp.github.io/FSharp.Compiler.Service/untypedtree.html
[4]: https://github.com/fsprojects/fantomas
[5]: https://github.com/Microsoft/msbuild/blob/adb180d394176f36aca1cc2eac4455fef564739f/src/Tasks/Microsoft.Common.CurrentVersion.targets#L3407
[6]: https://github.com/7sharp9/falanx
[7]: https://web.archive.org/web/20190427014042/https://7sharp9.github.io/2015/07/12/2015-07-08-meta-matic/
[8]: https://github.com/fsprojects/FSharp.TypeProviders.SDK
[9]: https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/
[10]: https://developers.google.com/protocol-buffers/
[11]: https://github.com/mausch/Fleece
[12]: https://github.com/ctaggart/froto
[13]: https://github.com/eiriktsarpalis/QuotationCompiler
[14]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters
[15]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/code-quotations
[16]: https://github.com/ctaggart/FsAst
[17]: https://github.com/7sharp9/myriad
[18]: https://www.youtube.com/channel/UC0kXc1f_WBYSklrElcPWzgg
[19]: https://haxe.org
[20]: https://haxe.org/manual/macro-reification-expression.html
[21]: https://docs.microsoft.com/en-us/dotnet/core/tools/?tabs=netcore2x
[22]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions
[23]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records
