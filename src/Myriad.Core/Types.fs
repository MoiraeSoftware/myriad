namespace Myriad.Core

open System
open System.Collections.Generic
open FSharp.Compiler.SyntaxTree

type MyriadGeneratorAttribute(name: string) =
    inherit Attribute()
    member _.Name = name

type GeneratorContext =
    {   ConfigKey: string option
        ConfigGetter: string -> (string * obj) seq
        InputFilename: string
        AdditionalParameters: IDictionary<string, string> }
    
    static member Create(configKey, configHandler, inputFile, additionalParams) =
        { ConfigKey = configKey
          ConfigGetter = configHandler
          InputFilename = inputFile
          AdditionalParameters = additionalParams }

[<RequireQualifiedAccess>]
type Output =
    | Ast of SynModuleOrNamespace list
    | Source of string

type IMyriadGenerator =
    abstract member ValidInputExtensions: string seq
    abstract member Generate: GeneratorContext -> Output
