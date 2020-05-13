module Program

open Expecto

[<EntryPoint>]
let main args =
  runTestsWithCLIArgs [] args Tests.tests