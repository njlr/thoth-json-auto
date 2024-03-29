module Thoth.Json.Auto.Tests.Tuples

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
    "Tuples"
    [
      test "Decode.auto works for tuples 1" {
        let decoder = Decode.auto ()

        let json =
          """
          [
            123,
            "abc"
          ]
          """

        let result = Decode.fromString decoder json
        let actual = Expect.wantOk result ""

        Expect.equal actual (123, "abc") ""
      }

      test "Encode.auto works for tuples 1" {
        let encoder = Encode.auto ()

        let input = (123, "abc")

        let actual = Encode.toString 0 (encoder input)

        let expected =
          """
          [
            123,
            "abc"
          ]
          """

        Expect.jsonEqual actual expected ""
      }

      test "Decode.auto works for tuples 2" {
        let decoder = Decode.auto ()

        let json =
          """
          [
            "x",
            7,
            true
          ]
          """

        let result = Decode.fromString decoder json
        let actual = Expect.wantOk result ""

        Expect.equal actual ("x", 7, true) ""
      }

      test "Decode.auto works for tuples 3" {
        let decoder = Decode.auto ()

        let json =
          """
          [
            false,
            "i",
            42,
            true,
            "foobar"
          ]
          """

        let result = Decode.fromString decoder json
        let actual = Expect.wantOk result ""

        Expect.equal actual (false, "i", 42, true, "foobar") ""
      }
    ]
