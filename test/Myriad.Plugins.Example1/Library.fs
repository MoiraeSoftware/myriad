namespace Myriad.Plugins.Example1

open Myriad.Core

[<MyriadSdkGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGen with
        member __.DoThings() = "ok"
