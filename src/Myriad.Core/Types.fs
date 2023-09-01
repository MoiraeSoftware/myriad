namespace Myriad.Core

open System
open System.Collections.Generic
open Fantomas.FCS.Syntax

type MyriadGeneratorAttribute(name: string) =
    inherit Attribute()
    member _.Name = name

type ProjectContext =
    { project: string
      projectPath: string
      refs: string array
      compileBefore: string array
      compile: string array
      compileAfter: string array
      defineConstants: string array }

type GeneratorContext =
    { ConfigKey: string option
      ConfigGetter: string -> (string * obj) seq
      InputFilename: string
      ProjectContext: ProjectContext option
      AdditionalParameters: IDictionary<string, string> }

    static member Create(configKey, configHandler, inputFile, projectContext, additionalParams) =
        { ConfigKey = configKey
          ConfigGetter = configHandler
          InputFilename = inputFile
          ProjectContext = projectContext
          AdditionalParameters = additionalParams }

[<RequireQualifiedAccess>]
type Output =
    | Ast of SynModuleOrNamespace list
    | Source of string

type IMyriadGenerator =
    abstract member ValidInputExtensions : string seq
    abstract member Generate : GeneratorContext -> Output
