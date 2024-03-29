namespace Thoth.Json.Auto.Tests

open Thoth.Json.Core

#if FABLE_COMPILER
open Fable.Mocha
open Thoth.Json.JavaScript
#else
open Expecto
open Thoth.Json.Newtonsoft
#endif

[<RequireQualifiedAccess>]
module internal Decode =

  let value : Decoder<Json> =
    let jsonToJson (helpers : IDecoderHelpers<'json>) (json : 'json) =
      let rec loop (json : 'json) =
        if helpers.isNullValue json then
          Json.Null
        elif helpers.isString json then
          helpers.asString json
          |> Json.String
        elif helpers.isBoolean json then
          helpers.asBoolean json
          |> Json.Boolean
        elif helpers.isArray json then
          helpers.asArray json
          |> Array.map loop
          |> Json.Array
        elif helpers.isIntegralValue json then
          helpers.asFloat json
          |> uint
          |> Json.IntegralNumber
        elif helpers.isObject json then
          seq {
            for prop in helpers.getProperties json do
              let value =
                helpers.getProperty(prop, json)
                |> loop

              prop, value
          }
          |> Json.Object
        else
          failwith $"Unexpected JSON %A{json}"

      loop json

    {
      new Decoder<Json> with
        member this.Decode(helpers, json : 'json) =
          jsonToJson helpers json
          |> Ok
    }

[<RequireQualifiedAccess>]
module internal Expect =

  let private normalizeJson (x : string) =
    match Decode.fromString Decode.value x with
    | Ok x -> Encode.toString 0 x
    | Error _ -> x

  let jsonEqual (actual : string) (expected : string) message =
    Expect.equal (normalizeJson actual) (normalizeJson expected) message

#if FABLE_COMPILER
  let private printVerses a b c d = sprintf "%s:\n%s\n%s:\n%s" a b c d

  let private printSeq xs = xs |> Seq.map string |> String.concat ", "

  let private firstDiff s1 s2 =
    let s1 = Seq.append (Seq.map Some s1) (Seq.initInfinite (fun _ -> None))
    let s2 = Seq.append (Seq.map Some s2) (Seq.initInfinite (fun _ -> None))
    Seq.mapi2 (fun i s p -> i,s,p) s1 s2
    |> Seq.find (function |_,Some s,Some p when s=p -> false |_-> true)

  /// Expects the `actual` sequence to equal the `expected` one.
  let sequenceEqual actual expected message =
    let baseMsg() = printVerses "expected" (printSeq expected) "  actual" (printSeq actual)
    match firstDiff actual expected with
    | _,None,None -> ()
    | i,Some a, Some e ->
      failtestf "%s. Sequence does not match at position %i. Expected item: %A, but got %A.%s"
        message i e a (baseMsg())
    | i,None,Some e ->
      failtestf "%s. Sequence actual shorter than expected, at pos %i for expected item %A.%s"
        message i e (baseMsg())
    | i,Some a,None ->
      failtestf "%s. Sequence actual longer than expected, at pos %i found item %A.%s"
        message i a (baseMsg())
#endif
