namespace Example3

open Myriad.Plugins


[<Generator.DUKind>]
type TestDU =
    | DUcase0 of int
    | DUcase1 of int
    | DUcase2 of string
    | DUcase3 of string*int
    | DUcase4
