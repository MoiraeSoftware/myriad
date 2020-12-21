namespace Myriad.Core

open System
open FSharp.Compiler.SyntaxTree

type MyriadGeneratorAttribute(name: string) =
    inherit Attribute()
    member __.Name = name

type IMyriadGenerator =
    abstract member ValidInputExtensions: string seq
    abstract member Generate: myriadConfigKey: string option * configGetter:(string -> (string * obj) seq) * inputFilename: string -> FsAst.AstRcd.SynModuleOrNamespaceRcd list
