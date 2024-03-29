module Thoth.Json.Auto.Tests.Seq

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
    "Seq"
    [
      test "Decode.auto works for seq 1" {
        let decoder = Decode.auto ()

        let cases =
          [
            "[]", Seq.empty
            "[ 7 ]", seq { 7 }
            "[ 1, 2, 3 ]", seq { 1; 2; 3 }
          ]

        for json, expected in cases do
          let result = Decode.fromString decoder json
          let actual = Expect.wantOk result ""

          Expect.sequenceEqual actual expected ""
      }

      test "Encode.auto works for seq 1" {
        let encoder = Encode.auto ()

        let cases =
          [
            Seq.empty, "[]"
            seq { 7 }, "[7]"
            seq { 1; 2; 3 }, "[1,2,3]"
          ]

        for input, expected in cases do
          let actual = Encode.toString 0 (encoder input)

          Expect.sequenceEqual actual expected ""
      }
    ]