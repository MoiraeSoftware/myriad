namespace Myriad.Core

open System
open Microsoft.FSharp.Compiler.Ast
open FsAst

type MyriadGenAttribute(generator: string) =
    inherit Attribute()
    
    member __.Generator = generator

type MyriadSdkGeneratorAttribute(name: string) =
    inherit Attribute()

    member __.Name = name

type IMyriadGen =
    abstract member Generate: namespace': string * ast:ParsedInput -> SynModuleOrNamespaceRcd