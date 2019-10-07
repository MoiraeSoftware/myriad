namespace Myriad.Core

open System

type MyriadGenAttribute(generator: string) =
    inherit Attribute()
    
    member __.Generator = generator

type MyriadSdkGeneratorAttribute(name: string) =
    inherit Attribute()

    member __.Name = name

type IMyriadGen =
    abstract member DoThings: unit -> string
