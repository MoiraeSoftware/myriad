# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.8.2]
### Changed
Updated FSharp.Compiler.Service to 41.0.6 Thanks @AlexeyRaga
### Fixed
MyriadSdk path is now quoted which fixes #159 and allows the path to have spaces.  Thanks @lucasteles

## [0.8.1]
### Changed
- Added support for generator filters with msbuild property `<Generators>...</Generators>` only the named generators are run, all others are excluded, the Generators element is added under the Compile element as follows:
    ```
        <Compile Include="Input.fs">
            <MyriadParams>
                <MyriadParam1>1</MyriadParam1>
                <MyriadParam2>2</MyriadParam2>
            </MyriadParams>
            <Generators>LensesGenerator;FieldsGenerator;DUCasesGenerator</Generators>
        </Compile>
    ```

- If `MyriadConfigFile` is not specified assume it would be: `$(ProjectDirectory)\myriad.toml`.
- If there is no config file present an empty one is created.


## [0.8.0]
### Changed
- Updated to dotnet 6.0.202
- Various improvements to lens generation code

## [0.7.4]
### Changed
- Exceptions are no longer written to the generated files but are instead reported to the command line
With the advent on in line generation, errors out to the source code can be quite disruptive


## [0.7.3]
### Added
- Added ProjectPath to the project context so the full name of the project is know by the plugin rather than just its project name
- Added contextual loading of each plugin so transient assemblies are loaded relative to the plugin location.

## [0.7.2]
### Added
- Added netcoreapp3.0 target for the Myriad CLI as theres no CLI version of netstandard2.0

## [0.7.1]
### Added
- Added netstandard2.0 target

## [0.7.0]
### Added
- Added a project context to Myriad context that comprises all of the data 
available at pre compile time that is useful to code generation, especially 
when generating a typed Ast.  
- Added more extensions to Ast nodes so they can be constructed easier.  
### Changed
- Altered the generation code to use plain Ast nodes rather then wrapper records, in 
the long term depreciating Rcd prefixed types from FsAst, simple extensions are preferred 
and less verbose.  
### Breaking
- Altered the plugin API so that both Ast and string based output are possible via a 
discriminated union return type.  

## [0.6.4]
### Added
- More Ast extensions to make creating Ast nodes easier.  

## [0.6.3]
### Fixed
- Added stack trace to failing code generation.  

## [0.6.2]
### Added
- Cache invalidation has been improved for inline generation, both file hash and time stamp of 
the input file are now considered.

## [0.6.1]
### Fixed
- Targets file not using correct Using references.  
- Inline generation not using the input file prior to myriad generation being appended.  

## [0.6.0]
### Added
- Add Support for inline code generation, generated code appears at the end of the input file.
### Changed
- Removed Fornax
- Removed Fake
- Updated to dotnet5.0
- Rewrote the build script as msbuild

## [0.5.4]
- Add support from config arguments to be specified in the msbuild config via the sub elements 
using: <MyriadParams><Param>test</Param></MyriadParams>
- Add support for custom arguments via the --additionalparam myparam=test;myotherparam=test2
- Update FSAst to 0.9.0
- Add support for Aether via pipedsetters=true in myriad config for lens

## [0.5.2]
- Update FSAst to 0.8.0 - Thanks! @theangrybyrd

## [0.5.1]
### Added
- A global config file can now be specified in fsproj: `<MyriadConfigFile>someglobalconfig.toml</MyriadConfigFile>`
- The DU plugin can now use the `alwaysFullyQualify = true` setting to force qualification of DU's
### Fixed
- RequireQualifiedAccess is now respected by the DU plugin - Thanks! @matthewcrews

## [0.5.0]
###Added
- Allow toml config to be used in plugins
- Allow arbitrary inputs to be used
- Added a config key that can be added per MyriadFile iteration or as a CLI parameter
### Changed
- Depreciated usage of MyriadNamespace in MSBuild

## [0.4.1]
- Update to use netstandard 2.0 for maximum compatibility

## [0.4.0]
- Update FSAst 0.6, Fantomas 4.0.0-beta-001, FSharp.Compiler.Service 36.0.3

## [0.3.1]
- Added lens generation

## [0.2.8]
### Changed
- Updated to latest version of FsAst 0.5.0

## [0.2.7]
### Added
- Verbose output flag added to CLI tool for diagnostics
- PDB info is embedded in plugins for better debugging experience
- Updated compiler and fantomas assemblies

## [0.2.6]
### Added
- Update CI infrastructure and release pipeline

## [0.2.5]
### Added
- New feature - DuCases generator added
- New feature - Added map function to Fields plugin

## [0.2.4]
### Changed
- Fixed Example via direct reference to Myriad.Core

### Removed
- Remove duplicate ItemGroup reference

## [0.2.3]
### Changed
- Corrected issue with plugin targets using PropertyGroup rather than ItemGroup

## [0.2.2]
### Changed
- Fixed typo in msbuild cache

### Added
- Added core plugin to target props as this look to be not included in the nuget package

## [0.2.1]
### Added
- Added diagnostics

## [0.2.0]
### Changed
- Updated to dotnet 3.0

### Added
- Added Plugin API

## [0.1.0] - 2019-04-19
### Changed
- Stopped using Fields as a nested module.
- Opened namespace of enclosing record so references are found.
