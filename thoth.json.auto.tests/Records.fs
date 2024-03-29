module Thoth.Json.Auto.Tests.Records

open Expecto
open Thoth.Json.Auto

#if FABLE_COMPILER
open Fable.Mocha
open Thoth.Json.JavaScript
#else
open Expecto
open Thoth.Json.Newtonsoft
#endif

type private Price =
  {
    Cents : int
  }

type private LineItem =
  {
    Sku : string
    Quantity : int
  }

type private Book =
  {
    Title : string
    Author : string
    Year : int
  }

type private Comment =
  {
    Content : string
    Replies : Comment list
  }

let tests =
  testList
    "Records"
    [
      test "Decode.auto works for records 1" {
        let decoder = Decode.auto ()

        let json =
          """
          {
            "Cents": 99
          }
          """

        let expected =
          {
            Cents = 99
          }

        let result = Decode.fromString decoder json
        let actual = Expect.wantOk result ""

        Expect.equal actual expected ""
      }

      test "Encode.auto works for records 1" {
        let encoder = Encode.auto ()

        let input =
          {
            Cents = 20
          }

        let actual = Encode.toString 0 (encoder input)

        let expected =
          """
          {
            "Cents": 20
          }
          """

        Expect.jsonEqual actual expected ""
      }

      test "Decode.auto works for records 2" {
        let decoder = Decode.auto ()

        let json =
          """
          {
            "Sku": "abc-def",
            "Quantity": 180
          }
          """

        let expected =
          {
            Sku =  "abc-def"
            Quantity = 180
          }

        let result = Decode.fromString decoder json
        let actual = Expect.wantOk result ""

        Expect.equal actual expected ""
      }

      test "Encode.auto works for records 2" {
        let encoder = Encode.auto ()

        let input =
          {
            Sku =  "abc-def"
            Quantity = 180
          }

        let actual = Encode.toString 0 (encoder input)

        let expected =
          """
          {
            "Sku": "abc-def",
            "Quantity": 180
          }
          """

        Expect.jsonEqual actual expected ""
      }

      test "Decode.auto works for records 3" {
        let decoder = Decode.auto ()

        let json =
          """
          {
            "Title": "Player of Games",
            "Author": "Iain M. Banks",
            "Year": 1988
          }
          """

        let expected =
          {
            Title = "Player of Games"
            Author = "Iain M. Banks"
            Year = 1988
          }

        let result = Decode.fromString decoder json
        let actual = Expect.wantOk result ""

        Expect.equal actual expected ""
      }

      test "Encode.auto works for records 3" {
        let encoder = Encode.auto ()

        let input =
          {
            Title = "Player of Games"
            Author = "Iain M. Banks"
            Year = 1988
          }

        let actual = Encode.toString 0 (encoder input)

        let expected =
          """
          {
            "Title": "Player of Games",
            "Author": "Iain M. Banks",
            "Year": 1988
          }
          """

        Expect.jsonEqual actual expected ""
      }

      test "Decode.auto works for records 4" {
        let decoder = Decode.auto ()

        let json =
          """
          {
            "Content": "abc",
            "Replies": [
              {
                "Content": "def",
                "Replies": [
                  {
                    "Content": "ghi",
                    "Replies": []
                  }
                ]
              },
              {
                "Content": "jkl",
                "Replies": []
              }
            ]
          }
          """

        let expected =
          {
            Content = "abc"
            Replies =
              [
                {
                  Content = "def"
                  Replies = [
                    {
                      Content = "ghi"
                      Replies = []
                    }
                  ]
                }
                {
                  Content = "jkl"
                  Replies = []
                }
              ]
          }

        let result = Decode.fromString decoder json
        let actual = Expect.wantOk result ""

        Expect.equal actual expected ""
      }

      test "Encode.auto works for records 4" {
        let encoder = Encode.auto ()

        let input =
          {
            Content = "abc"
            Replies =
              [
                {
                  Content = "def"
                  Replies = [
                    {
                      Content = "ghi"
                      Replies = []
                    }
                  ]
                }
                {
                  Content = "jkl"
                  Replies = []
                }
              ]
          }

        let actual = Encode.toString 0 (encoder input)

        let expected =
          """
          {
            "Content": "abc",
            "Replies": [
              {
                "Content": "def",
                "Replies": [
                  {
                    "Content": "ghi",
                    "Replies": []
                  }
                ]
              },
              {
                "Content": "jkl",
                "Replies": []
              }
            ]
          }
          """

        Expect.jsonEqual actual expected ""
      }

      test "Decode.auto works for records with case style 1" {
        let decoder =
          Decode.autoWithOptions
            (AutoOptions.empty |> AutoOptions.withCaseStyle ScreamingSnakeCase)

        let json =
          """
          {
            "CENTS": 99
          }
          """

        let expected =
          {
            Cents = 99
          }

        let result = Decode.fromString decoder json
        let actual = Expect.wantOk result ""

        Expect.equal actual expected ""
      }

      test "Encode.auto works for records with case style 1" {
        let encoder =
          Encode.autoWithOptions
            (AutoOptions.empty |> AutoOptions.withCaseStyle CamelCase)

        let input =
          {
            Cents = 20
          }

        let actual = Encode.toString 0 (encoder input)

        let expected =
          """
          {
            "cents": 20
          }
          """

        Expect.jsonEqual actual expected ""
      }
    ]
