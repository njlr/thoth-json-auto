namespace Thoth.Json.Auto

open System
open Thoth.Json.Core

type CaseStyle =
  | SnakeCase
  | ScreamingSnakeCase
  | PascalCase
  | CamelCase
  | DotNetPascalCase
  | DotNetCamelCase

type TypeKey =
  private | TypeKey of string
  static member internal Create(t : Type) =
    TypeKey t.FullName

[<RequireQualifiedAccess>]
module TypeKey =

  let ofType (t : Type) =
    TypeKey.Create(t)

type AutoOptions =
  {
    CaseStyle : CaseStyle option
    EncoderOverrides : Map<TypeKey, obj>
    DecoderOverrides : Map<TypeKey, obj>
  }

[<RequireQualifiedAccess>]
module AutoOptions =

  let empty =
    {
      CaseStyle = None
      EncoderOverrides = Map.empty
      DecoderOverrides = Map.empty
    }

  let withCaseStyle (caseStyle : CaseStyle) (opts : AutoOptions) =
    {
      opts with
        CaseStyle = Some caseStyle
    }

  let withoutCaseStyle (opts : AutoOptions) =
    {
      opts with
        CaseStyle = None
    }

  let overrideEncoderImpl (typeKey : TypeKey) (encoder : obj) (opts : AutoOptions) =
    {
      opts with
        EncoderOverrides =
          opts.EncoderOverrides
          |> Map.add typeKey encoder
    }

  let inline overrideEncoder (encoder : Encoder<'t>) (opts : AutoOptions) =
    overrideEncoderImpl (TypeKey.ofType typeof<'t>) encoder opts

  let overrideDecoderImpl (typeKey : TypeKey) (decoder : obj) (opts : AutoOptions) =
    {
      opts with
        DecoderOverrides =
          opts.DecoderOverrides
          |> Map.add typeKey decoder
    }

  let inline overrideDecoder (decoder : Decoder<'t>) (opts : AutoOptions) =
    overrideDecoderImpl (TypeKey.ofType typeof<'t>) decoder opts
