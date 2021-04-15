# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
