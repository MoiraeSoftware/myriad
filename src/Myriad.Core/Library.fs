namespace Myriad.Core

open System

type MyriadGenAttribute(generator: string) =
    inherit Attribute()

type IMyriadGen =
    abstract member GetName: unit -> string
