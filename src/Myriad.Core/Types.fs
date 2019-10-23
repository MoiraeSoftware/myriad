namespace Myriad.Core

open System
open Microsoft.FSharp.Compiler.Ast
open FsAst

type MyriadGenerateAttribute(generator: string) =
    inherit Attribute()
    
    member __.Generator = generator

type MyriadGeneratorAttribute(name: string) =
    inherit Attribute()

    member __.Name = name

type IMyriadGenerator =
    abstract member Generate: namespace': string * ast:ParsedInput -> SynModuleOrNamespaceRcd