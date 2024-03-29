module Thoth.Json.Auto.Tests.Array

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
    "Array"
    [
      test "Decode.auto works for array 1" {
        let decoder = Decode.auto ()

        let cases =
          [
            "[]", [||]
            "[ 7 ]", [| 7 |]
            "[ 1, 2, 3 ]", [| 1; 2; 3 |]
          ]

        for json, expected in cases do
          let result = Decode.fromString decoder json
          let actual = Expect.wantOk result ""

          Expect.equal actual expected ""
      }

      test "Encode.auto works for array 1" {
        let encoder = Encode.auto ()

        let cases =
          [
            [||], "[]"
            [| 7 |], "[7]"
            [| 1; 2; 3 |], "[1,2,3]"
          ]

        for input, expected in cases do
          let actual = encoder input |> Encode.toString 0

          Expect.sequenceEqual actual expected ""
      }
    ]