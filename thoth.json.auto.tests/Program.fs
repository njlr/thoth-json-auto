module Thoth.Json.Auto.Tests.Entry

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let tests =
  testList
    "Thoth.Json.Auto"
    [
      Casing.tests
      Primitives.tests
      Tuples.tests
      Option.tests
      List.tests
      Array.tests
      Seq.tests
      Records.tests
      Unions.tests
      Overrides.tests
    ]

[<EntryPoint>]
let main argv =
#if FABLE_COMPILER
  Mocha.runTests tests
#else
  runTestsWithCLIArgs [] argv tests
#endif
