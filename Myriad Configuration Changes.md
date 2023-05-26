---
title: "Myriad Configuration Changes"
description: "New Configuration Changes for Myriad"
author: 7sharp9
date: 2022-05-15
categories: ["programming"]
showRecent: true
tags: ["fsharp", "myriad", "metaprogramming", "configuration"]
---
The other day I released version [0.8.1][1] of [Myriad][2], its got some new configuration settings so I thought I would quickly talk about them here.  <!--more-->

## New Configuration Features  
The main change in [0.8.1][1] is that it is now possible to specify filters so that only certain generators can run rather than all generators that are found withing a plugin or plugins that are present in your project file.  This is done by adding the `Generators` element to your Myriad compile element within your project file.  

## Adding a plugin  

Briefly before we get onto the new configuration I mentioned, I just wanted to quickly cover the two way plugins are normally added to project.  Plugins can be either enabled via a Nuget mechanism or by including an msbuild import that imports them.  The Nuget method is what I would recommend as it means less editing of the project file and the plugin author would normally  specify that it exports the plugin for Myriad to consume.  The plugin author would include the following in a `Myriad.Plugins.MyPlugin.props` file in their project which would then be exported as part of the Nuget build:

```xml
<Project>
    <ItemGroup>
        <MyriadSdkGenerator Include="$(MSBuildThisFileDirectory)/../lib/net6.0/Myriad.Plugins.MyPlugin.dll" />
    </ItemGroup>
</Project>
```

So when consuming the plugin via Nuget the msbuild property is included with your project and Myriad knows about the plugin assembly.

If you are testing or are not using Nuget for whatever reason then you can import the plugin manually into your project by adding the import yourself like this:

```xml
<Import Project="..\Myriad.Plugins.Example1\build\Myriad.Plugins.Example1.InTest.props" />
```

This is mainly used for testing where you would not want to create a local nuget package just to test or debug something.  

When Myriad runs it searches for all plugins by looking at the assemblies specified by `MyriadSdkGenerator` and then finding all instances of types that implement the interface `IMyriadGenerator`.  

```fsharp
[<RequireQualifiedAccess>]
type Output =
    | Ast of SynModuleOrNamespace list
    | Source of string

type IMyriadGenerator =
    abstract member ValidInputExtensions : string seq
    abstract member Generate : GeneratorContext -> Output
```

Any assemblies specified by `MyriadSdkGenerator` that have types that implement `IMyriadGenerator` are then ran and their generated output added to your project at the relevant places as specified by the configuration properties.  I will go further into this aspect of configuration in a further post.  Im currently working on full documentation but its a bit easier to write a series of blog posts as it forces you to start writing about all the little rabbit holes you fall down whole while describing the process, whereas documentation tends to read more linearly and implicit knowledge can sometimes be skipped over.  

## The Generators configuration

As you read above, all generators which implement `IMyriadGenerator` in any of the plugins are executed.  This is normally ok but sometimes there might be a plugin which does not need file input and is driven by an external file or process, or there might be more than one plugin per assembly, this means that plugins have to be more defensively coded so they cannot be run if their relevant inputs are not satisfied.  Its good practice to defensively code any plugins anyway as you would not want to cause an unrecoverable exception in the build process and instead want the plugin to fail gracefully.  In the case of a Myriad plugin, if the plugin cannot run or the inputs result in an error, diagnostic output occurs and the plugin gracefully returns.  This means Myriad can continue on with the next generators.  The problem with this is its a little wasteful, ahead of compilation time you often know which plugins you want to run so the new configuration element `Generators` solves this issue.

If the `Generators` element is present then the semicolon separated list of filters are passed to Myriad so that only those named generators are ran.  Lets look at the `Generators` element in a snipped of a build file:


```xml
    <Compile Include="Input.fs">
        <MyriadParams>
            <MyriadParam1>1</MyriadParam1>
            <MyriadParam2>2</MyriadParam2>
        </MyriadParams>
        <Generators>LensesGenerator;FieldsGenerator;DUCasesGenerator</Generators>
    </Compile>
```

This is part of the integration tests in Myriad, when `Input.fs` is compiled it is passed to any plugins that have generators.   Within Myriad there are two plugin assemblies `Myriad.Plugins.dll` which contains the following generators: `LensesGenerator`, `FieldsGenerator`, and `DUCasesGenerator`.  There is also another plugin assembly called `Myriad.Plugins.Example1` which contains a generator called `example1` which only operates on `.txt` extensions so when that plugin runs it wont have any valid input to generate for so it makes sense to exclude this plugin completely.  Adding the element `<Generators>LensesGenerator;FieldsGenerator;DUCasesGenerator</Generators>` allows you to pass this information to Myriad so ir will filter out all generators that are not named in the `Generators` element.  

The new configuration gives you more flexibility in controlling which plugins do and don't get run and as a result Myriad will be faster and more efficient as it will get run for the generators specified in the filter.  Don't worry though if the `Generators` element is not present then all generators will get run as usual.

I hoped you enjoyed reading a little about the new configuration and a small about of background relating to it.

Thanks for reading, Until next time!


[1]: https://www.nuget.org/packages/myriad
[2]: https://github.com/MoiraeSoftware/myriad
