module Thoth.Json.Auto.Tests.Overrides

open Thoth.Json.Core
open Thoth.Json.Auto

#if FABLE_COMPILER
open Fable.Mocha
open Thoth.Json.JavaScript
#else
open Expecto
open Thoth.Json.Newtonsoft
#endif

type private Foo =
  {
    Bar : string
  }

let tests =
  testList
    "Overrides"
    [
      test "Encode.auto applies overrides" {
        let weirdStringEncoder =
          fun x ->
            Encode.string (x + "!!!")

        let opts =
          AutoOptions.empty
          |> AutoOptions.overrideEncoder weirdStringEncoder

        let encoder = Encode.autoWithOptions opts

        let actual = encoder { Bar = "Hello" } |> Encode.toString 0

        let expected =
          """
          {
            "Bar": "Hello!!!"
          }
          """

        Expect.jsonEqual actual expected ""
      }

      test "Decode.auto applies overrides" {
        let weirdStringDecoder =
          Decode.string
          |> Decode.map (fun x -> x.ToUpper())

        let opts =
          AutoOptions.empty
          |> AutoOptions.overrideDecoder weirdStringDecoder

        let decoder = Decode.autoWithOptions opts

        let json =
          """
          {
            "Bar": "hello"
          }
          """

        let decoded = Decode.fromString decoder json
        let actual = Expect.wantOk decoded ""

        let expected =
          {
            Bar = "HELLO"
          }

        Expect.equal actual expected ""
      }
    ]
