module Toml.Parser.Tests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Test exposing (..)
import Toml
import Toml.Parser exposing (parse)


basicTests : Test
basicTests =
    [ ( "empty"
      , ""
      , Just []
      )
    , ( "single keypair"
      , "key = true"
      , Just [ ( "key", Toml.Bool True ) ]
      )
    , ( "single comment"
      , "# comment"
      , Just []
      )
    , ( "two keypairs"
      , "key1 = 'val1'\nkey2 = 'val2'"
      , Just
            [ ( "key1", Toml.String "val1" )
            , ( "key2", Toml.String "val2" )
            ]
      )
    , ( "comment at eol"
      , "key = true  #  a comment goes here"
      , Just [ ( "key", Toml.Bool True ) ]
      )
    , ( "no duplicate keys"
      , "key = true\nkey = false"
      , Nothing
      )
    ]
        |> List.map makeTest
        |> describe "basic tests"


keyFormats : Test
keyFormats =
    [ ( "bare key"
      , "key = ''"
      , Just [ ( "key", Toml.String "" ) ]
      )
    , ( "literal key"
      , "'key 123' = ''"
      , Just [ ( "key 123", Toml.String "" ) ]
      )
    , ( "regular key"
      , "\"key foo 'bar'\" = ''"
      , Just [ ( "key foo 'bar'", Toml.String "" ) ]
      )
    , ( "dotted bare key"
      , "parent.child = true"
      , Just [ ( "parent", table [ ( "child", Toml.Bool True ) ] ) ]
      )
    , ( "whitespace between parts"
      , "parent\t.  child= false"
      , Just [ ( "parent", table [ ( "child", Toml.Bool False ) ] ) ]
      )
    , ( "combination of key formats"
      , "'foo'.bar.\"baz\" = ''"
      , Just
            [ ( "foo"
              , table
                    [ ( "bar"
                      , table
                            [ ( "baz"
                              , Toml.String ""
                              )
                            ]
                      )
                    ]
              )
            ]
      )
    ]
        |> List.map makeTest
        |> describe "key formats"


boolValue : Test
boolValue =
    [ ( "parse true"
      , "true"
      , Just (Toml.Bool True)
      )
    , ( "parse false"
      , "false"
      , Just (Toml.Bool False)
      )
    , ( "case sensitive"
      , "True"
      , Nothing
      )
    ]
        |> List.map makeValueTest
        |> describe "boolean values"


binaryIntValue : Test
binaryIntValue =
    [ ( "0"
      , "0b0"
      , Just (Toml.Int 0)
      )
    , ( "1"
      , "0b1"
      , Just (Toml.Int 1)
      )
    , ( "longer"
      , "0b00101"
      , Just (Toml.Int 5)
      )
    , ( "longer with underscores"
      , "0b00_1_1"
      , Just (Toml.Int 3)
      )
    , ( "empty"
      , "0b"
      , Nothing
      )
    , ( "non-binary"
      , "0b2"
      , Nothing
      )
    , ( "start with underscore"
      , "0b_1"
      , Nothing
      )
    , ( "trailing underscore"
      , "0b1_"
      , Nothing
      )
    ]
        |> List.map makeValueTest
        |> describe "binary integer values"


octalIntValue : Test
octalIntValue =
    [ ( "0"
      , "0o0"
      , Just (Toml.Int 0)
      )
    , ( "1"
      , "0o1"
      , Just (Toml.Int 1)
      )
    , ( "7"
      , "0o7"
      , Just (Toml.Int 7)
      )
    , ( "longer"
      , "0o01234567"
      , Just (Toml.Int 342391)
      )
    , ( "longer with underscores"
      , "0o10_4_7"
      , Just (Toml.Int 551)
      )
    , ( "empty"
      , "0o"
      , Nothing
      )
    , ( "non-octal"
      , "0o8"
      , Nothing
      )
    , ( "start with underscore"
      , "0o_1"
      , Nothing
      )
    , ( "trailing underscore"
      , "0o1_"
      , Nothing
      )
    ]
        |> List.map makeValueTest
        |> describe "octal integer values"


hexIntValue : Test
hexIntValue =
    [ ( "0"
      , "0x0"
      , Just (Toml.Int 0)
      )
    , ( "1"
      , "0x1"
      , Just (Toml.Int 1)
      )
    , ( "7"
      , "0x7"
      , Just (Toml.Int 7)
      )
    , ( "longer"
      , "0x01234567"
      , Just (Toml.Int 19088743)
      )
    , ( "longer with underscores"
      , "0x10_4_7"
      , Just (Toml.Int 4167)
      )
    , ( "deadbeef"
      , "0xdeadbeef"
      , Just (Toml.Int 3735928559)
      )
    , ( "dead_BEEF"
      , "0xdead_BEEF"
      , Just (Toml.Int 3735928559)
      )
    , ( "DEADBEEF"
      , "0xDEADBEEF"
      , Just (Toml.Int 3735928559)
      )
    , ( "empty"
      , "0x"
      , Nothing
      )
    , ( "non-hex"
      , "0xg"
      , Nothing
      )
    , ( "start with underscore"
      , "0x_1"
      , Nothing
      )
    , ( "trailing underscore"
      , "0x1_"
      , Nothing
      )
    ]
        |> List.map makeValueTest
        |> describe "hexadecimal integer values"


literalIntValue : Test
literalIntValue =
    [ ( "0"
      , "0"
      , Just (Toml.Int 0)
      )
    , ( "+0"
      , "+0"
      , Just (Toml.Int 0)
      )
    , ( "-0"
      , "-0"
      , Just (Toml.Int 0)
      )
    , ( "123"
      , "123"
      , Just (Toml.Int 123)
      )
    , ( "-9999909"
      , "-9999909"
      , Just (Toml.Int -9999909)
      )
    , ( "underscores allowed"
      , "123_456"
      , Just (Toml.Int 123456)
      )
    , ( "no leading 0"
      , "012"
      , Nothing
      )
    , ( "no double sign"
      , "--12"
      , Nothing
      )
    , ( "no leading underscore"
      , "_12"
      , Nothing
      )
    , ( "no trailing underscore"
      , "12_"
      , Nothing
      )
    ]
        |> List.map makeValueTest
        |> describe "literal integer values"


fractionalFloat : Test
fractionalFloat =
    [ ( "0.0"
      , "0.0"
      , Just (Toml.Float 0)
      )
    , ( "-0.0"
      , "-0.0"
      , Just (Toml.Float 0)
      )
    , ( "+0.0"
      , "+0.0"
      , Just (Toml.Float 0)
      )
    , ( "-123.456"
      , "-123.456"
      , Just (Toml.Float -123.456)
      )
    , ( "0.001"
      , "0.001"
      , Just (Toml.Float 0.001)
      )
    , ( "underscores"
      , "9_224_617.445_991_228_313"
      , Just (Toml.Float 9224617.445991227)
      )
    ]
        |> List.map makeValueTest
        |> describe "fractional float values"


weirdFloats : Test
weirdFloats =
    let
        extractVal : Toml.Document -> Result String Float
        extractVal doc =
            case Dict.get "key" doc of
                Just (Toml.Float v) ->
                    Ok v

                _ ->
                    Err "key not found or not a Float"
    in
    describe "edge case float tests"
        [ test "infinity" <|
            \_ ->
                Toml.Parser.parse "key = inf"
                    |> Result.mapError toString
                    |> Result.andThen extractVal
                    |> Result.map (\x -> isInfinite x && x > 0)
                    |> Expect.equal (Ok True)
        , test "positive infinity" <|
            \_ ->
                Toml.Parser.parse "key = +inf"
                    |> Result.mapError toString
                    |> Result.andThen extractVal
                    |> Result.map (\x -> isInfinite x && x > 0)
                    |> Expect.equal (Ok True)
        , test "negative infinity" <|
            \_ ->
                Toml.Parser.parse "key = -inf"
                    |> Result.mapError toString
                    |> Result.andThen extractVal
                    |> Result.map (\x -> isInfinite x && x < 0)
                    |> Expect.equal (Ok True)
        , test "nan" <|
            \_ ->
                Toml.Parser.parse "key = nan"
                    |> Result.mapError toString
                    |> Result.andThen extractVal
                    |> Result.map isNaN
                    |> Expect.equal (Ok True)
        , test "pos nan" <|
            \_ ->
                Toml.Parser.parse "key = +nan"
                    |> Result.mapError toString
                    |> Result.andThen extractVal
                    |> Result.map isNaN
                    |> Expect.equal (Ok True)
        , test "neg nan" <|
            \_ ->
                Toml.Parser.parse "key = -nan"
                    |> Result.mapError toString
                    |> Result.andThen extractVal
                    |> Result.map isNaN
                    |> Expect.equal (Ok True)
        ]


suite : Test
suite =
    describe "to be sorted"
        [ test "skips empty lines" <|
            \_ ->
                """
key1 = 'val1'

key2 = 'val2'
       """
                    |> parse
                    |> succeed
                        [ ( "key1", Toml.String "val1" )
                        , ( "key2", Toml.String "val2" )
                        ]
        , test "skips comments between pairs" <|
            \_ ->
                """
key1 = 'val1'

# This is a comment
       # This is an indented comment
key2 = 'val2'
       """
                    |> parse
                    |> succeed
                        [ ( "key1", Toml.String "val1" )
                        , ( "key2", Toml.String "val2" )
                        ]
        , test "Fail: adding to a non-table" <|
            \_ ->
                """
key = 'not a table'
key.child = 'nope'
            """
                    |> parse
                    |> fail
        , test "Nested friends" <|
            \_ ->
                """
key.child1 = 'child 1'
key.child2 = 'child 2'
             """
                    |> parse
                    |> succeed
                        [ ( "key"
                          , table
                                [ ( "child1", Toml.String "child 1" )
                                , ( "child2", Toml.String "child 2" )
                                ]
                          )
                        ]
        ]



-- helpers


makeValueTest : ( String, String, Maybe Toml.Value ) -> Test
makeValueTest ( name, input, result ) =
    makeTest
        ( name
        , "key = " ++ input
        , Maybe.map (\x -> [ ( "key", x ) ]) result
        )


makeTest : ( String, String, Maybe (List ( String, Toml.Value )) ) -> Test
makeTest ( name, input, res ) =
    let
        expectation : Result e Toml.Document -> Expectation
        expectation =
            res
                |> Maybe.map succeed
                |> Maybe.withDefault fail
    in
    test name <|
        \_ ->
            parse input
                |> expectation


doc : List ( String, Toml.Value ) -> Toml.Document
doc =
    Dict.fromList


table : List ( String, Toml.Value ) -> Toml.Value
table =
    doc >> Toml.Table


succeed : List ( String, Toml.Value ) -> Result e Toml.Document -> Expectation
succeed v =
    Expect.equal (Ok (doc v))


fail : Result e v -> Expectation
fail =
    Result.mapError (always ())
        >> Expect.equal (Err ())
