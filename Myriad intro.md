---
author: 7sharp9
title: "Myriad Intro"
description: "New Meta Programming Club Video - An Introduction to Myriad"
categories: ["programming"]
date: 2019-11-06T17:08:32Z
tags: ["fsharp", "youtube", "videos", "myriad", "metaprogramming", "quotations", "ast"]
---

I released a new meta programming club [video][1] yesterday on my YouTube [channel][2]

<iframe width="560" height="315" src="https://www.youtube.com/embed/Yef8EJJfd9Q" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

I thought I would write a little about it here on my blog too.

I recently did a little work to flesh out the plugin interface for Myriad.  My friend [Enrico Sada][5] helped out with 
some of the fiddly MsBuild work in dotnet which can be a little confusing at times.  

The gist of the plugin is that you must implement the following interface:

```
type IMyriadGenerator =
    abstract member Generate: namespace': string * ast:ParsedInput -> SynModuleOrNamespaceRcd
```  

And annotate you type with the `MyriadGeneratorAttribute` specifying the name of your plugin so that it can be found.  
Heres an example plugin from the [Myriad repo][4]:

```
[<MyriadGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGenerator with
        member __.Generate(namespace', _ast) =

            let let42 =
                SynModuleDecl.CreateLet
                    [ { SynBindingRcd.Let with
                            Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "fourtyTwo", [])
                            Expr = SynExpr.CreateConst(SynConst.Int32 42) } ]

            let componentInfo = SynComponentInfoRcd.Create [ Ident.Create "example1" ]
            let nestedModule = SynModuleDecl.CreateNestedModule(componentInfo, [ let42 ])

            let namespaceOrModule =
                { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with Declarations = [ nestedModule ] }

            namespaceOrModule
```

All this example does is generate a simple module like this:

```
module example1 =
    let fourtyTwo = 42
```

### Ast Helping Hand

Unfortunately working with the AST can be quite verbose in Myriad I use [FsAst][6] which uses the record update syntax to aid
in the construction of AST nodes by using the record update syntax so you don't have to supply every parameter.  

Take a Let binding AST element:

```
{SynBindingRcd.Let with
    Pattern = pattern
    Expr = expr }
```

If we did not use the record update syntax in `SynBindingRcd` then we would have to supply every parameter for the let binding:

```
{
    Access = None
    Kind = SynBindingKind.NormalBinding
    IsInline = false
    IsMutable = false
    Attributes = SynAttributes.Empty
    XmlDoc = PreXmlDoc.Empty
    ValData = SynValData(Some MemberFlags.InstanceMember, SynValInfo.Empty, None)
    Pattern = pattern
    ReturnInfo = None
    Expr = expr
    Range = range.Zero
    Bind = SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding }
```

Which is not exactly fun, the record update syntax makes these type of things easy to define and compose together.

- - -  
  
The new version of Myriad can be found on Nuget [here][3] or at its [repo][4].  

I hope you enjoyed the video and this brief blog!

Until next time!  

[1]: https://youtu.be/Yef8EJJfd9Q
[2]: https://www.youtube.com/channel/UC0kXc1f_WBYSklrElcPWzgg
[3]: https://www.nuget.org/packages/Myriad.Sdk/0.2.4
[4]: https://github.com/MoiraeSoftware/myriad
[5]: https://twitter.com/enricosada
[6]: https://github.com/ctaggart/FsAst
