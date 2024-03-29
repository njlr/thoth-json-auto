module Thoth.Json.Auto.Tests.Unions

open Thoth.Json.Auto

#if FABLE_COMPILER
open Fable.Mocha
open Thoth.Json.JavaScript
#else
open Expecto
open Thoth.Json.Newtonsoft
#endif

type private Traffic =
  | Red
  | Amber
  | Green

type private Area =
  | Unbounded
  | Circle of int
  | Square of int
  | Rectangle of int * int

type private Tree =
  | Leaf of int
  | Branch of Tree * Tree

let tests =
  testList
    "Unions"
    [
      test "Decode.auto works for unions 1" {
        let decoder = Decode.auto ()

        let cases =
          [
            "\"Red\"", Traffic.Red
            "\"Amber\"", Traffic.Amber
            "\"Green\"", Traffic.Green
          ]

        for json, expected in cases do
          let result = Decode.fromString decoder json
          let actual = Expect.wantOk result $"Failed to decode %s{json}"

          Expect.equal actual expected ""
      }

      test "Encode.auto works for unions 1" {
        let encoder = Encode.auto ()

        let cases =
          [
            Traffic.Red, "\"Red\""
            Traffic.Amber, "\"Amber\""
            Traffic.Green, "\"Green\""
          ]

        for input, expected in cases do
          let actual = Encode.toString 0 (encoder input)

          Expect.jsonEqual actual expected ""
      }

      test "Decode.auto works for unions 2" {
        let decoder = Decode.auto ()

        let cases =
          [
            "\"Unbounded\"", Unbounded
            "[ \"Circle\", 1 ]", Circle 1
            "[ \"Square\", 2 ]", Square 2
            "[ \"Rectangle\", 3, 4 ]", Rectangle (3, 4)
          ]

        for json, expected in cases do
          let result = Decode.fromString decoder json
          let actual = Expect.wantOk result $"Failed to decode %s{json}"

          Expect.equal actual expected ""
      }

      test "Encode.auto works for unions 2" {
        let encoder = Encode.auto ()

        let cases =
          [
            Unbounded, "[ \"Unbounded\" ]"
            Circle 1, "[ \"Circle\", 1 ]"
            Square 2, "[ \"Square\", 2 ]"
            Rectangle (3, 4), "[ \"Rectangle\", 3, 4 ]"
          ]

        for input, expected in cases do
          let actual = Encode.toString 0 (encoder input)

          Expect.jsonEqual actual expected ""
      }

      test "Decode.auto works for unions 3" {
        let decoder = Decode.auto ()

        let cases =
          [
            "[ \"Leaf\", 2 ]", Leaf 2
            "[ \"Branch\", [ \"Leaf\", 3 ], [ \"Leaf\", 4 ] ]", Branch (Leaf 3, Leaf 4)
            (
              "[ \"Branch\", [ \"Branch\", [ \"Leaf\", 1 ], [ \"Leaf\", 2 ] ], [ \"Leaf\", 3 ] ]",
              Branch (Branch (Leaf 1, Leaf 2), Leaf 3)
            )
          ]

        for json, expected in cases do
          let result = Decode.fromString decoder json
          let actual = Expect.wantOk result $"Failed to decode %s{json}"

          Expect.equal actual expected ""
      }

      test "Encode.auto works for unions 3" {
        let encoder = Encode.auto ()

        let cases =
          [
            Leaf 2, "[ \"Leaf\", 2 ]"
            Branch (Leaf 3, Leaf 4), "[ \"Branch\", [ \"Leaf\", 3 ], [ \"Leaf\", 4 ] ]"
            (
              Branch (Branch (Leaf 1, Leaf 2), Leaf 3),
              "[ \"Branch\", [ \"Branch\", [ \"Leaf\", 1 ], [ \"Leaf\", 2 ] ], [ \"Leaf\", 3 ] ]"
            )
          ]

        for input, expected in cases do
          let actual = Encode.toString 0 (encoder input)

          Expect.jsonEqual actual expected ""
      }
    ]
