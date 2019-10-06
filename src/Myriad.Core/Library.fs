namespace Myriad.Core

open System

type MyriadGenAttribute(generator: string) =
    inherit Attribute()


type MyriadSdkGeneratorAttribute(name: string) =
    inherit Attribute()

    member __.Name = name

type IMyriadGen =
    abstract member DoThings: unit -> string
