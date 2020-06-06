namespace Myriad.Core

open System
open FSharp.Compiler.SyntaxTree

type MyriadGeneratorAttribute() =
    inherit Attribute()

type IMyriadGenerator =
    abstract member Generate: configGetter:(string -> (string * obj) seq) * ast:ParsedInput -> FsAst.AstRcd.SynModuleOrNamespaceRcd list