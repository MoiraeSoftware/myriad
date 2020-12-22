namespace Myriad.Core

open System
open FSharp.Compiler.SyntaxTree

type MyriadGeneratorAttribute(name: string) =
    inherit Attribute()
    member __.Name = name

type GeneratorContext = {
    ConfigKey: string option
    ConfigGetter: string -> (string * obj) seq
    InputFileName: string
}

type IMyriadGenerator =
    abstract member ValidInputExtensions: string seq
    abstract member Generate: GeneratorContext -> FsAst.AstRcd.SynModuleOrNamespaceRcd list
