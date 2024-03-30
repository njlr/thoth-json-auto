module Thoth.Json.Auto.Tests.Primitives

open System
open System.Globalization
open Thoth.Json.Auto

#if FABLE_COMPILER
open Fable.Mocha
open Thoth.Json.JavaScript
#else
open Expecto
open Thoth.Json.Newtonsoft
#endif

let encode =
  testList
    "Encode"
    [
      test "Encode.auto works for int" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder 123)

        Expect.equal actual "123" ""
      }

      test "Encode.auto works for char" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder 'x')

        Expect.equal actual "\"x\"" ""
      }

      test "Encode.auto works for string" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder "abc")

        Expect.equal actual "\"abc\"" ""
      }

      test "Encode.auto works for bool" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder true)

        Expect.equal actual "true" ""
      }

      test "Encode.auto works for byte" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder 123uy)

        Expect.equal actual "123" ""
      }

      test "Encode.auto works for sbyte" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder 96y)

        Expect.equal actual "96" ""
      }

      test "Encode.auto works for uint16" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder 256us)

        Expect.equal actual "256" ""
      }

      // test "Encode.auto works for int16" {
      //   let encoder = Encode.auto ()

      //   let actual = Encode.toString 0 (encoder -7s)

      //   Expect.equal actual "-7" ""
      // }

      test "Encode.auto works for int64" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder 1234567L)

        Expect.equal actual "\"1234567\"" ""
      }

      test "Encode.auto works for uint" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder 1234u)

        Expect.equal actual "1234" ""
      }

      test "Encode.auto works for bigint" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder 1234567I)

        Expect.equal actual "\"1234567\"" ""
      }

      test "Encode.auto works for decimal" {
        let encoder = Encode.auto ()

        let actual = Encode.toString 0 (encoder 1234567.89M)

        Expect.equal actual "\"1234567.89\"" ""
      }

      test "Encode.auto works for Guid" {
        let encoder = Encode.auto ()

        let actual =
          encoder (Guid.Parse "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11")
          |> Encode.toString 0

        Expect.equal actual "\"a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11\"" ""
      }

#if !FABLE_COMPILER
      test "Encode.auto works for TimeSpan" {
        let encoder = Encode.auto ()

        let actual =
          encoder (TimeSpan.Parse "1.02:03:04.5670000")
          |> Encode.toString 0

        Expect.equal actual "\"1.02:03:04.5670000\"" ""
      }
#endif

      test "Encode.auto works for DateTime" {
        let encoder = Encode.auto ()

        let actual =
          encoder (DateTime.Parse("2019-01-01T12:34:56.789", CultureInfo.InvariantCulture))
          |> Encode.toString 0

#if FABLE_COMPILER
        let expected = "\"2019-01-01T12:34:56.789\""
#else
        let expected = "\"2019-01-01T12:34:56.7890000\""
#endif

        Expect.equal actual expected ""
      }

      test "Encode.auto works for DateTimeOffset" {
        let encoder = Encode.auto ()

        let actual =
          encoder (DateTimeOffset.Parse("2022-03-29T14:12:47.6258038+01:00", CultureInfo.InvariantCulture))
          |> Encode.toString 0

#if FABLE_COMPILER
        let expected = "\"2022-03-29T14:12:47.625+01:00\""
#else
        let expected = "\"2022-03-29T14:12:47.6258038+01:00\""
#endif

        Expect.equal actual expected ""
      }
    ]

let decode =
  testList
    "Decode"
    [
      test "Decode.auto works for int" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "123"
        let actual = Expect.wantOk result ""

        Expect.equal actual 123 ""
      }

      test "Decode.auto works for char" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "\"a\""
        let actual = Expect.wantOk result ""

        Expect.equal actual 'a' ""
      }

      test "Decode.auto works for string" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "\"abc\""
        let actual = Expect.wantOk result ""

        Expect.equal actual "abc" ""
      }

      test "Decode.auto works for bool" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "true"
        let actual = Expect.wantOk result ""

        Expect.equal actual true ""
      }

      test "Decode.auto works for byte" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "96"
        let actual = Expect.wantOk result ""

        Expect.equal actual 96uy ""
      }

      test "Decode.auto works for sbyte" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "92"
        let actual = Expect.wantOk result ""

        Expect.equal actual 92y ""
      }

      test "Decode.auto works for uint16" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "123"
        let actual = Expect.wantOk result ""

        Expect.equal actual 123us ""
      }

      test "Decode.auto works for int16" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "123"
        let actual = Expect.wantOk result ""

        Expect.equal actual 123s ""
      }

      test "Decode.auto works for uint" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "123"
        let actual = Expect.wantOk result ""

        Expect.equal actual 123u ""
      }

      test "Decode.auto works for int64" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "123456789"
        let actual = Expect.wantOk result ""

        Expect.equal actual 123456789L ""
      }

      test "Decode.auto works for bigint" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "123456789"
        let actual = Expect.wantOk result ""

        Expect.equal actual 123456789I ""
      }

      test "Decode.auto works for decimal" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "123456789"
        let actual = Expect.wantOk result ""

        Expect.equal actual 123456789m ""
      }

      test "Decode.auto works for Guid" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "\"2dd674b7-0c13-40d3-af53-89d1a6d176c3\""
        let actual = Expect.wantOk result ""

        Expect.equal actual (Guid.Parse "2dd674b7-0c13-40d3-af53-89d1a6d176c3") ""
      }

      test "Decode.auto works for TimeSpan" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "\"00:00:00.0001234\""
        let actual = Expect.wantOk result ""

        Expect.equal actual (TimeSpan.Parse "00:00:00.0001234") ""
      }

      test "Decode.auto works for DateTime" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "\"2023-07-22T15:35:21.226\""
        let actual = Expect.wantOk result ""

        let expected = DateTime.Parse("2023-07-22T15:35:21.226", CultureInfo.InvariantCulture).ToUniversalTime()

        Expect.equal actual expected ""
      }

#if !FABLE_COMPILER
      test "Decode.auto works for DateTimeOffset" {
        let decoder = Decode.auto ()

        let result = Decode.fromString decoder "\"2023-07-22T15:35:21.226\""
        let actual = Expect.wantOk result ""

        let expected = DateTimeOffset.Parse("2023-07-22T15:35:21.226", CultureInfo.InvariantCulture).ToUniversalTime()

        Expect.equal actual expected ""
      }
#endif
    ]

let tests =
  testList
    "Primitives"
    [
      encode
      decode
    ]
