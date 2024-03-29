module Thoth.Json.Auto.Tests.Casing

open Thoth.Json.Auto
open Thoth.Json.Auto.Casing

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let tests =
  testList "Casing" [
    test "convertCase works 1" {
      let actual = convertCase ScreamingSnakeCase SnakeCase "FRUIT_SALAD"
      let expected = "fruit_salad"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 2" {
      let actual = convertCase SnakeCase PascalCase "aphex_twin"
      let expected = "AphexTwin"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 3" {
      let actual = convertCase ScreamingSnakeCase PascalCase "MEAT_PUPPETS"
      let expected = "MeatPuppets"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 4" {
      let actual = convertCase SnakeCase DotNetPascalCase "product_id"
      let expected = "ProductID"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 5" {
      let actual = convertCase DotNetPascalCase ScreamingSnakeCase "ProductID"
      let expected = "PRODUCT_ID"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 6" {
      let actual = convertCase CamelCase ScreamingSnakeCase "id"
      let expected = "ID"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 7" {
      let actual = convertCase CamelCase DotNetPascalCase "id"
      let expected = "ID"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 8" {
      let actual = convertCase CamelCase DotNetCamelCase "productId"
      let expected = "productID"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 9" {
      let actual = convertCase ScreamingSnakeCase DotNetCamelCase "ID"
      let expected = "id"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 10" {
      let actual = convertCase SnakeCase DotNetPascalCase "id"
      let expected = "ID"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 11" {
      let actual = convertCase PascalCase DotNetPascalCase "OIDCProvider"
      let expected = "OidcProvider"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 12" {
      let actual = convertCase PascalCase DotNetCamelCase "OIDCProvider"
      let expected = "oidcProvider"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 13" {
      let actual = convertCase PascalCase DotNetPascalCase "Identifier"
      let expected = "Identifier"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 14" {
      let actual = convertCase PascalCase DotNetCamelCase "Identifier"
      let expected = "identifier"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 15" {
      let actual = convertCase CamelCase DotNetPascalCase "updateIpAllowListEnabledSettingInput"
      let expected = "UpdateIPAllowListEnabledSettingInput"

      Expect.equal actual expected "The string should be as expected"
    }

    test "convertCase works 16" {
      let actual = convertCase PascalCase CamelCase "FooBar"
      let expected = "fooBar"

      Expect.equal actual expected "The string should be as expected"
    }
  ]
