module Thoth.Json.Auto.Tests.Option

open Thoth.Json.Auto

#if FABLE_COMPILER
open Fable.Mocha
open Thoth.Json.JavaScript
#else
open Expecto
open Thoth.Json.Newtonsoft
#endif

let tests =
  testList
    "Option"
    [
      test "Decode.auto works for option 1" {
        let decoder = Decode.auto ()

        let cases =
          [
            "null", None
            "123", Some 123
          ]

        for json, expected in cases do
          let result = Decode.fromString decoder json
          let actual = Expect.wantOk result ""

          Expect.equal actual expected ""
      }

      test "Encode.auto works for option 1" {
        let encoder = Encode.auto ()

        let cases =
          [
            None, "null"
            Some 123, "123"
          ]

        for input, expected in cases do
          let actual = encoder input |> Encode.toString 0

          Expect.equal actual expected ""
      }
    ]
