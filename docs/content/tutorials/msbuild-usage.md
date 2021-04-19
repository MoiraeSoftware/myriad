---
title: MSBuild usage
category: tutorial
menu_order: 2
---

# MSBuild usage

Plugins for Myriad are supplied by simply including the nuget package in your project, the nuget infrastructure supplies the necessary MSBuild props and targets so that the plugin is used by Myriad automatically. Following the source for the fields plugin can be used as reference until more details about authoring plugins is created.

To use Myriad via its MSBuild support you add the `Myriad.Core` and `Myriad.Sdk` package references:
```xml
    <ItemGroup>
      <PackageReference Include="Myriad.Core" Version="0.5.1" />
      <PackageReference Include="Myriad.Sdk" Version="0.5.1" />
      <PackageReference Include="Myriad.Plugins" Version="0.5.1" /> <!-- Built in set of plugins -->
    </ItemGroup>
```

An input file is specified by using the usual `Compile` element:
```xml
<Compile Include="Generated.fs">
    <MyriadFile>Library.fs</MyriadFile>
</Compile>
```

This is configuring Myriad so that a file called `Generated.fs` will be included in the build using `Library.fs` as input.  

Myriad works by using plugins to generate code.  A plugin called fields is included with Myriad which takes inspiration from OCamls [ppx_fields_conv](https://github.com/janestreet/ppx_fields_conv) plugin of the same name.  There are also plugins included to generate discriminated union helpers adn record lenses.
