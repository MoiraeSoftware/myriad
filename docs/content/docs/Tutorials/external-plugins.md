---
title: External Plugins
category: tutorial
menu_order: 3
pre: "<i class='fas fa-external-link-alt'></i> "

---

# Using external plugins

To consume external plugins that aren't included in the `Myriad.Plugins` package, you must register them with Myriad. If you are using the CLI tool then the way to do this is by passing in the `--plugin <path to dll>` command-line argument. If you are using MSBuild then this can be done by adding to the `MyriadSdkGenerator` property to your project file:

```xml
<ItemGroup>
    <MyriadSdkGenerator Include="<path to plugin dll>" />
</ItemGroup>
```

For example, if you had a project layout like this:

```
\src
-\GeneratorLib
 - Generator.fs
 - Generator.fsproj
-\GeneratorTests
 - Tests.fs
 - GeneratorTests.fsproj
```

You would add the following to Generator.fsproj:
```xml
  <ItemGroup>
    <Content Include="build\Generator.props">
      <Pack>true</Pack>
      <PackagePath>%(Identity)</PackagePath>
      <Visible>true</Visible>
    </Content>
  </ItemGroup>
```

Then add a new folder `build` with the `Generator.props` file within:
```xml
<Project>
    <ItemGroup>
        <MyriadSdkGenerator Include="$(MSBuildThisFileDirectory)/../lib/netstandard2.1/Generator.dll" />
    </ItemGroup>
</Project>
```

Often an additional props file (In this smaple the file would be `Generator.InTest.props`) is used to make testing easier.  The matching element for the tests fsproj would be something like this:

```xml
<Project>
    <ItemGroup>
        <MyriadSdkGenerator Include="$(MSBuildThisFileDirectory)/../bin/$(Configuration)/netstandard2.1/Generator.dll" />
    </ItemGroup>
</Project>
```

Notice the Include path is pointing locally rather than within the packaged nuget folder structure.

In your testing `fsproj` you would add the following to allow the plugin to be used locally rather that having to consume a nuget package:

```xml
<!-- include plugin -->
<Import Project="<Path to Generator plugin location>\build\Myriad.Plugins.InTest.props" />
