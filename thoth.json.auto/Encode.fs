namespace Thoth.Json.Auto

open System
open System.Reflection
open FSharp.Reflection
open Thoth.Json.Core
open Thoth.Json.Auto

[<RequireQualifiedAccess>]
module Encode =

  [<RequireQualifiedAccess>]
  module internal Encode =

    [<RequireQualifiedAccess>]
    module Generic =

      type EncodeHelpers =
        static member OptionOf<'t>(enc : Encoder<'t>) : Encoder<'t option> =
          Encode.option enc

        static member Lazily<'t>(enc : Lazy<Encoder<'t>>) : Encoder<'t> =
          Encode.lazily enc

        static member SeqOf<'t>(enc : Encoder<'t>) : Encoder<'t seq> =
          fun xs ->
            xs
            |> Seq.map enc
            |> Seq.toArray
            |> Encode.array

        static member ListOf<'t>(enc : Encoder<'t>) : Encoder<'t list> =
          fun xs ->
            xs |> List.map enc |> Encode.list

        static member ArrayOf<'t>(enc : Encoder<'t>) : Encoder<'t array> =
          fun xs ->
            xs
            |> Array.map enc
            |> Encode.array

        // static member Field<'t, 'u>(picker : 't -> 'u, fieldEncoder : Encoder<'u>) : Encode.IFieldEncoder<'t> =
        //   Encode.field picker fieldEncoder

        // static member Object<'t>(fields : seq<string * Encode.IFieldEncoder<'t>>) : Encoder<'t> =
        //   Encode.object fields

        // static member Element<'t, 'u>(picker : 't -> 'u, elementEncoder : Encoder<'u>) : Encoder<'t> =
        //   Encode.element picker elementEncoder

        // static member FixedArray<'t>(elements : Encoder<'t> seq) : Encoder<'t> =
        //   Encode.fixedArray elements

        // static member Union<'t>(picker : 't -> Encode.ICase<'t>) : Encoder<'t> =
        //   Encode.union picker

        // static member Case<'t>(tag : string, data : Encode.ICaseData<'t> seq) : Encode.ICase<'t> =
        //   Encode.case tag data

#if FABLE_COMPILER
      let optionOf (innerType : Type) (enc : obj) : obj =
        Encode.option (unbox enc)
#else
      let private getGenericMethodDefinition (name : string) =
        typeof<EncodeHelpers>.GetMethods(BindingFlags.Static ||| BindingFlags.NonPublic)
        |> Seq.filter (fun x -> x.Name = name)
        |> Seq.exactlyOne
        |> fun mi -> mi.GetGenericMethodDefinition()

      let private optionOfMethodDefinition = getGenericMethodDefinition "OptionOf"

      let optionOf (innerType : Type) (enc : obj) : obj =
        let methodInfo = optionOfMethodDefinition.MakeGenericMethod(innerType)
        methodInfo.Invoke(null, [| enc |])
#endif

#if FABLE_COMPILER
      let seqOf (innerType : Type) (enc : obj) : obj =
        box (fun xs -> unbox xs |> Seq.map (unbox enc) |> Encode.seq)
#else
      let private seqOfMethodDefinition = getGenericMethodDefinition "SeqOf"

      let seqOf (innerType : Type) (enc : obj) : obj =
        let methodInfo = seqOfMethodDefinition.MakeGenericMethod(innerType)
        methodInfo.Invoke(null, [| enc |])
#endif

#if FABLE_COMPILER
      let listOf (innerType : Type) (enc : obj) : obj =
        box (fun xs -> unbox xs |> List.map (unbox enc) |> Encode.list)
#else
      let private listOfMethodDefinition = getGenericMethodDefinition "ListOf"

      let listOf (innerType : Type) (enc : obj) : obj =
        let methodInfo = listOfMethodDefinition.MakeGenericMethod(innerType)
        methodInfo.Invoke(null, [| enc |])
#endif

#if FABLE_COMPILER
      let arrayOf (innerType : Type) (enc : obj) : obj =
        EncodeHelpers.ArrayOf(unbox enc)
#else
      let private arrayOfMethodDefinition = getGenericMethodDefinition "ArrayOf"

      let arrayOf (innerType : Type) (enc : obj) : obj =
        let methodInfo = arrayOfMethodDefinition.MakeGenericMethod(innerType)
        methodInfo.Invoke(null, [| enc |])
#endif

#if FABLE_COMPILER
      let lazily (innerType : Type) (enc : obj) : obj =
        Encode.lazily (unbox enc)
#else
      let private lazilyMethodDefinition = getGenericMethodDefinition "Lazily"

      let lazily (innerType : Type) (enc : obj) : obj =
        let methodInfo = lazilyMethodDefinition.MakeGenericMethod(innerType)
        methodInfo.Invoke(null, [| enc |])
#endif

      // let private fieldMethodDefinition = getGenericMethodDefinition "Field"

      // let field (objectType : Type) (fieldType : Type) (picker : obj) (fieldEncoder : obj) : obj =
      //   let methodInfo = fieldMethodDefinition.MakeGenericMethod(objectType, fieldType)
      //   methodInfo.Invoke(null, [| picker; fieldEncoder |])

      // let private objectMethodDefinition = getGenericMethodDefinition "Object"

      // let object (objectType : Type) (fields : obj) : obj =
      //   let methodInfo = objectMethodDefinition.MakeGenericMethod(objectType)
      //   methodInfo.Invoke(null, [| fields |])

      // let private elementMethodDefinition = getGenericMethodDefinition "Element"

      // let element (objectType : Type) (elementType : Type) (picker : obj) (elementEncoder : obj) : obj =
      //   let methodInfo = elementMethodDefinition.MakeGenericMethod(objectType, elementType)
      //   methodInfo.Invoke(null, [| picker; elementEncoder |])

      // let private fixedArrayMethodDefinition = getGenericMethodDefinition "FixedArray"

      // let fixedArray (objectType : Type) (elements : obj) : obj =
      //   let methodInfo = fixedArrayMethodDefinition.MakeGenericMethod(objectType)
      //   methodInfo.Invoke(null, [| elements |])

      // let private unionMethodDefinition = getGenericMethodDefinition "Union"

      // let union (objectType : Type) (picker : obj) : obj =
      //   let methodInfo = unionMethodDefinition.MakeGenericMethod(objectType)
      //   methodInfo.Invoke(null, [| picker |])

      // let private caseMethodDefinition = getGenericMethodDefinition "Case"

      // let case (objectType : Type) (tag : string) (data : obj) : obj =
      //   let methodInfo = caseMethodDefinition.MakeGenericMethod(objectType)
      //   methodInfo.Invoke(null, [| tag; data |])

  // let private makeFieldEncoderType (ty : Type) : Type =
  //   typedefof<Encode.IFieldEncoder<obj>>.MakeGenericType(ty)

  // let private makeEncodeCaseType (ty : Type) : Type =
  //   typedefof<Encode.ICase<obj>>.MakeGenericType(ty)

#if !FABLE_COMPILER
  let private makeEncoderType (ty : Type) : Type =
    FSharpType.MakeFunctionType(ty, typeof<Json>)
    // typedefof<Encoder<obj>>.MakeGenericType(ty)
#endif

  let rec generateEncoder (caseStyle : CaseStyle option) (existingEncoders : Map<TypeKey, obj>) (ty : Type) : obj =
    match Map.tryFind (TypeKey.ofType ty) existingEncoders with
    | Some x -> x
    | None ->
      match ty with
      | IntType _ -> box Encode.int
      | CharType _ -> box Encode.char
      | StringType _ -> box Encode.string
      | BoolType _ -> box Encode.bool
      | ByteType _ -> box Encode.byte
      | SByteType _ -> box Encode.sbyte
      | UInt16Type _ -> box Encode.uint16
      | Int16Type _ -> box Encode.int16
      | Int64Type _ -> box Encode.int64
      | UIntType _ -> box Encode.uint32
      | BigIntType _ -> box Encode.bigint
      | DecimalType _ -> box Encode.decimal
      | GuidType _ -> box (fun (g : Guid) -> Encode.guid g)
#if !FABLE_COMPILER
      | TimeSpanType _ -> box (fun (ts : TimeSpan) -> Encode.timespan ts)
#endif
      | DateTimeType _ -> box Encode.datetime
      | DateTimeOffsetType _ -> box Encode.datetimeOffset
      | OptionType innerType ->
        let innerEncoder = generateEncoder caseStyle existingEncoders innerType
        Encode.Generic.optionOf innerType innerEncoder
      | SeqType innerType ->
        let innerEncoder = generateEncoder caseStyle existingEncoders innerType
        Encode.Generic.seqOf innerType innerEncoder
      | ListType innerType ->
        let innerEncoder = generateEncoder caseStyle existingEncoders innerType
        Encode.Generic.listOf innerType innerEncoder
      | ArrayType innerType ->
        let innerEncoder = generateEncoder caseStyle existingEncoders innerType
        Encode.Generic.arrayOf innerType innerEncoder
      | FSharpRecordType _ ->
        generateEncoderForRecord caseStyle existingEncoders ty
      | FSharpUnionType _ ->
        generateEncoderForUnion caseStyle existingEncoders ty
      | FSharpTupleType _ ->
        generateEncoderForTuple caseStyle existingEncoders ty
      | _ ->
        failwith $"Unsupported type %s{ty.FullName}"

  and private generateEncoderForRecord (caseStyle : CaseStyle option) (existingEncoders : Map<TypeKey, obj>) (ty : Type) : obj =
#if FABLE_COMPILER
    let mutable self = Unchecked.defaultof<_>

    let existingEncoders =
      if Type.isRecursive ty then
        let lazySelf =
          Encode.Generic.lazily
            ty
            (Lazy.makeGeneric
              (typeof<obj -> obj>)
              ((fun _ -> self)))

        existingEncoders
        |> Map.add (TypeKey.ofType ty) lazySelf
      else
        existingEncoders

    let recordFieldsWithEncoders =
      [|
        for pi in FSharpType.GetRecordFields(ty, allowAccessToPrivateRepresentation=true) do
          let fieldEncoder : obj -> Json =
            generateEncoder caseStyle existingEncoders pi.PropertyType
            |> unbox

          let reader =
            fun (record : obj) ->
              FSharpValue.GetRecordField(record, pi)

          let readAndEncode (record : obj) =
            let value = reader record
            fieldEncoder value

          pi.Name, readAndEncode
      |]

    let encoder : obj -> obj =
      fun o ->
        let fields =
          [|
            for fieldName, readAndEncode in recordFieldsWithEncoders do
              let encodedFieldName =
                match caseStyle with
                | Some caseStyle ->
                  Casing.convertCase DotNetPascalCase caseStyle fieldName
                | None -> fieldName

              encodedFieldName, readAndEncode o
          |]

        Encode.object fields

    self <- box encoder

    box encoder
#else
    let mutable self = Unchecked.defaultof<_>

    let existingEncoders =
      if Type.isRecursive ty then
        let lazySelf =
          Encode.Generic.lazily
            ty
            (Lazy.makeGeneric
              (makeEncoderType ty)
              (FSharpValue.MakeFunction(
                FSharpType.MakeFunctionType(typeof<unit>, makeEncoderType ty),
                (fun _ -> self))))

        existingEncoders
        |> Map.add (TypeKey.ofType ty) lazySelf
      else
        existingEncoders

    let funcType = makeEncoderType ty

    let recordFieldsWithEncoders =
      [|
        for pi in FSharpType.GetRecordFields(ty, allowAccessToPrivateRepresentation=true) do
          let fieldEncoder = generateEncoder caseStyle existingEncoders pi.PropertyType

          let invokeMethodInfo =
            fieldEncoder.GetType().GetMethods()
            |> Array.find (fun x -> x.Name = "Invoke" && x.ReturnType = typedefof<Json>)

          let reader = FSharpValue.PreComputeRecordFieldReader(pi)

          let readAndEncode (record : obj) =
            let value = reader record
            invokeMethodInfo.Invoke(fieldEncoder, [| value |])
            :?> Json

          pi.Name, readAndEncode
      |]

    let funcImpl : obj -> obj =
      fun o ->
        let fields =
          [|
            for fieldName, readAndEncode in recordFieldsWithEncoders do
              let encodedFieldName =
                match caseStyle with
                | Some caseStyle ->
                  Casing.convertCase DotNetPascalCase caseStyle fieldName
                | None -> fieldName

              encodedFieldName, readAndEncode o
          |]

        Encode.object fields

    let encoder = FSharpValue.MakeFunction(funcType, funcImpl)

    self <- encoder

    encoder
#endif

  and private generateEncoderForUnion (caseStyle : CaseStyle option) (existingEncoders : Map<TypeKey, obj>) (ty : Type) : obj =
#if FABLE_COMPILER
    let mutable self = Unchecked.defaultof<_>

    let existingEncoders =
      if Type.isRecursive ty then
        let lazySelf =
          Encode.Generic.lazily
            ty
            (Lazy.makeGeneric
              (typeof<obj -> obj>)
              ((fun _ -> self)))

        existingEncoders
        |> Map.add (TypeKey.ofType ty) lazySelf
      else
        existingEncoders

    let unionCases = FSharpType.GetUnionCases(ty, allowAccessToPrivateRepresentation=true)

    let anyCaseHasData =
      unionCases
      |> Array.exists (fun x -> x.GetFields() |> Seq.isEmpty |> not)

    let caseEncoders =
      [|
        for unionCase in unionCases do
          let encodedUnionCaseName =
            match caseStyle with
            | Some caseStyle ->
              Casing.convertCase DotNetPascalCase caseStyle unionCase.Name
            | None -> unionCase.Name

          if anyCaseHasData then
            let fieldEncoders =
              [|
                for pi in unionCase.GetFields() do
                  let encoder : obj -> Json =
                    generateEncoder caseStyle existingEncoders pi.PropertyType
                    |> unbox

                  encoder
              |]

            let n = Array.length fieldEncoders - 1

            fun o ->
              let _, values = FSharpValue.GetUnionFields(o, ty)

              Encode.array
                [|
                  Encode.string encodedUnionCaseName

                  for i = 0 to n do
                    let value = values[i]
                    let encoder : obj -> Json = unbox fieldEncoders[i]

                    encoder value
                |]
          else
            fun _ ->
              Encode.string encodedUnionCaseName
      |]

    let encoder : obj -> obj =
      fun o ->
        let caseInfo, _ = FSharpValue.GetUnionFields(o, ty)
        let tag = caseInfo.Tag
        let caseEncoder = caseEncoders[tag]

        caseEncoder o

    self <- encoder

    encoder
#else
    let mutable self = Unchecked.defaultof<_>

    let funcType = makeEncoderType ty

    let existingEncoders =
      if Type.isRecursive ty then
        let lazySelf =
          Encode.Generic.lazily
            ty
            (Lazy.makeGeneric
              (makeEncoderType ty)
              (FSharpValue.MakeFunction(
                FSharpType.MakeFunctionType(typeof<unit>, makeEncoderType ty),
                (fun _ -> self))))

        existingEncoders
        |> Map.add (TypeKey.ofType ty) lazySelf
      else
        existingEncoders

    let unionCases = FSharpType.GetUnionCases(ty, allowAccessToPrivateRepresentation=true)
    let tagReader = FSharpValue.PreComputeUnionTagReader(ty, allowAccessToPrivateRepresentation=true)

    let anyCaseHasData =
      unionCases
      |> Array.exists (fun x -> x.GetFields() |> Seq.isEmpty |> not)

    let caseEncoders =
      [|
        for unionCase in unionCases do
          let encodedUnionCaseName =
            match caseStyle with
            | Some caseStyle ->
              Casing.convertCase DotNetPascalCase caseStyle unionCase.Name
            | None -> unionCase.Name

          if anyCaseHasData then
            let unionReader = FSharpValue.PreComputeUnionReader(unionCase, allowAccessToPrivateRepresentation=true)

            let fieldEncoders =
              [|
                for pi in unionCase.GetFields() do
                  let encoder = generateEncoder caseStyle existingEncoders pi.PropertyType

                  let invokeMethodInfo =
                    encoder.GetType().GetMethods()
                    |> Array.find (fun x -> x.Name = "Invoke" && x.ReturnType = typeof<Json>)

                  fun o ->
                    invokeMethodInfo.Invoke(encoder, [| o |])
                    :?> Json
              |]

            let n = Array.length fieldEncoders - 1

            fun o ->
              let values = unionReader o

              Encode.array
                [|
                  Encode.string encodedUnionCaseName

                  for i = 0 to n do
                    let value = values[i]
                    let encoder = fieldEncoders[i]

                    encoder value
                |]
          else
            fun _ ->
              Encode.string encodedUnionCaseName
      |]

    let funcImpl : obj -> obj =
      fun o ->
        let tag = tagReader o
        let caseEncoder = caseEncoders[tag]

        caseEncoder o

    let encoder = FSharpValue.MakeFunction(funcType, funcImpl)

    self <- encoder

    encoder
#endif

  and private generateEncoderForTuple (caseStyle : CaseStyle option) (existingEncoders : Map<TypeKey, obj>) (ty : Type) : obj =
#if FABLE_COMPILER
    let encoders =
      [|
        for elementType in FSharpType.GetTupleElements(ty) do
          let elementEncoder = generateEncoder caseStyle existingEncoders elementType

          box elementEncoder
      |]

    let funcImpl : obj -> obj =
      fun o ->
        let values : ResizeArray<obj> = unbox o

        Encode.array
          [|
            for i = 0 to Array.length encoders - 1 do
              let value = unbox values[i]
              let encode : obj -> Json = unbox encoders[i]

              encode value
          |]

    box funcImpl
#else
    let funcType = makeEncoderType ty

    let reader = FSharpValue.PreComputeTupleReader(ty)

    let encoders =
      [|
        for elementType in FSharpType.GetTupleElements(ty) do
          let elementEncoder = generateEncoder caseStyle existingEncoders elementType

          let invokeMethodInfo =
            elementEncoder.GetType().GetMethods()
            |> Array.find (fun x -> x.Name = "Invoke" && x.ReturnType = typedefof<Json>)

          let encode (value : obj) =
            invokeMethodInfo.Invoke(elementEncoder, [| value |])
            :?> Json

          encode
      |]

    let n = Array.length encoders - 1

    let funcImpl : obj -> obj =
      fun o ->
        let values = reader o

        let elements =
          [|
            for i = 0 to n do
              let value = values[i]
              let encode = encoders[i]

              encode value
          |]

        Encode.array elements

    FSharpValue.MakeFunction(funcType, funcImpl)
#endif

  let inline autoWithOptions<'t> (options : AutoOptions) : Encoder<'t> =
    let ty = typeof<'t>
    let encoder = generateEncoder options.CaseStyle options.EncoderOverrides ty
    unbox encoder

  let inline auto<'t> () : Encoder<'t> =
    autoWithOptions AutoOptions.empty
