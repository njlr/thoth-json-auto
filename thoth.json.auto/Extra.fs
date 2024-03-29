namespace Thoth.Json.Auto

open System
open Thoth.Json.Core

[<AutoOpen>]
module internal Extra =

  [<RequireQualifiedAccess>]
  module Encode =

    let lazily (enc : Lazy<Encoder<'t>>) : Encoder<'t> =
      fun x -> enc.Value x

  [<RequireQualifiedAccess>]
  module Decode =

    type private LazyDecoder<'t>(x : Lazy<Decoder<'t>>) =
      struct end
      interface Decoder<'t> with
        member this.Decode<'json>(helpers : IDecoderHelpers<'json>, json : 'json) =
          let decoder = x.Force()
          decoder.Decode(helpers, json)

    let lazily (x : Lazy<Decoder<'t>>) : Decoder<'t> =
      LazyDecoder(x) :> _
