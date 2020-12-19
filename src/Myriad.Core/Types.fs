namespace Myriad.Core

open System
open FSharp.Compiler.SyntaxTree

type MyriadGeneratorAttribute() =
    inherit Attribute()

type IMyriadGenerator =
    abstract member ValidInputExtensions: string seq
    abstract member Generate: configGetter:(string -> (string * obj) seq) * inputFilename: string -> FsAst.AstRcd.SynModuleOrNamespaceRcd list
