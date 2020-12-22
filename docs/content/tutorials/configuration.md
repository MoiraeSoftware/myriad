---
title: Configuration And Plugins
category: tutorial
menu_order: 5
---

# Configuration

Myriad uses a configuration file called `myriad.toml`.  It uses the [toml format](https://toml.io/en/).  

The configuration as it stands for the built in plugins is very simple, the configuration name or key is passed in by either the plugins generator attribute, in the case of the fields plugin:

```
[<Generator.Fields "fields">]
type Test1 = { one: int; two: string; three: float; four: float32 }

```

The configuration key is "fields", if we now look at the `myriad.toml` file:

```toml
[fields]
namespace = "TestFields"
```

You can see that there is a `namespace` key that is set to "TestFields".  Using an attribute like this you can configure your plugin to take further information on how to generate the code.  

There is another mechanism of specifying what configuration a plugin will use and that is by using an MSBuild property `MyriadConfigKey` as follows:

```xml
<Compile Include="ArbitaryFile.fs">
    <MyriadFile>Test.txt</MyriadFile>
    <MyriadConfigKey>example1</MyriadConfigKey>
</Compile>
```

The configuration key `example1` would be passed to the plugin so that it could be consumed.  This is used in the example plugin `Myriad.Plugin.Example1` and use used again to specify the `namespace` used by the `example1` plugin.  The plugin interface receives this key via: `GeneratorContext` . `ConfigKey`:

```fsharp
type GeneratorContext = {
    ConfigKey: string option
    ConfigGetter: string -> (string * obj) seq
    InputFileName: string
}

type IMyriadGenerator =
    abstract member ValidInputExtensions: string seq
    abstract member Generate: GeneratorContext -> FsAst.AstRcd.SynModuleOrNamespaceRcd list
```

ValidInputExtensions is used so a plugin is tied to certain file types as input, if you plugin generating ast fragments from java files then you would add `.java` to the `ValidInputExtensions` implementation for example.  

```fsharp
    interface IMyriadGenerator with
        member __.ValidInputExtensions = seq {".java"}
        //...
```

While we are dicussing rhe IMyriadGenerator interface lets dicuss the `Generate` member:

Generate takes a `GeneratorContext` which contains the `ConfigKey` as mentioned above, the `ConfigGetter` : `string -> (string * obj) seq` which is a mean to allow access to the `myriad.toml` file, you give the function the configuration ket you wish to receive and it returns the configuration.  Finally a `InputFileName` is also passed in so you can load or parse your input files ready to generate an AST.  If you look at the included plugins you can see the mechanism for extracting and building AST fragments for reference.  