namespace Myriad.Core

open System
open FSharp.Compiler.Ast

type MyriadGenerateAttribute(generator: string) =
    inherit Attribute()

    member __.Generator = generator

type MyriadGeneratorAttribute(name: string) =
    inherit Attribute()

    member __.Name = name

type IMyriadGenerator =
    abstract member Generate: namespace': string * ast:ParsedInput -> FsAst.AstRcd.SynModuleOrNamespaceRcd