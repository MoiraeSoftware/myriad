namespace Myriad.Core

open System
open System.Collections.Generic

type MyriadGeneratorAttribute(name: string) =
    inherit Attribute()
    member _.Name = name

type GeneratorContext = {
    ConfigKey: string option
    ConfigGetter: string -> (string * obj) seq
    InputFilename: string
    AdditionalParameters: IDictionary<string, string>
}

type IMyriadGenerator =
    abstract member ValidInputExtensions: string seq
    abstract member Generate: GeneratorContext -> FsAst.AstRcd.SynModuleOrNamespaceRcd list
