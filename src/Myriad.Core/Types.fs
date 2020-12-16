namespace Myriad.Core

open System
open FSharp.Compiler.SyntaxTree

type MyriadGeneratorAttribute(name: string) =
    inherit Attribute()

    member __.Name = name

type IMyriadGenerator =
    abstract member Generate: namespace': string * inputFilename: string -> FsAst.AstRcd.SynModuleOrNamespaceRcd