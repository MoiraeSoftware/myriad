namespace Myriad.Plugins.Example1

open Myriad.Core

type Example1Gen() =
    interface IMyriadGen with
        member __.GetName() = "test1"
