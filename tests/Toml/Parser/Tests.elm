module Toml.Parser.Tests exposing (..)

import Array.Hamt as Array exposing (Array)
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
    makeValueTests "boolean values"
        Toml.Bool
        [ ( "parse true", "true", Just True )
        , ( "parse false", "false", Just False )
        , ( "case sensitive", "True", Nothing )
        ]


binaryIntValue : Test
binaryIntValue =
    makeValueTests "binary integer values"
        Toml.Int
        [ ( "0", "0b0", Just 0 )
        , ( "1", "0b1", Just 1 )
        , ( "longer", "0b00101", Just 5 )
        , ( "longer with underscores", "0b00_1_1", Just 3 )
        , ( "empty", "0b", Nothing )
        , ( "non-binary", "0b2", Nothing )
        , ( "start with underscore", "0b_1", Nothing )
        , ( "trailing underscore", "0b1_", Nothing )
        ]


octalIntValue : Test
octalIntValue =
    makeValueTests "octal integer values"
        Toml.Int
        [ ( "0", "0o0", Just 0 )
        , ( "1", "0o1", Just 1 )
        , ( "7", "0o7", Just 7 )
        , ( "longer", "0o01234567", Just 342391 )
        , ( "longer with underscores", "0o10_4_7", Just 551 )
        , ( "empty", "0o", Nothing )
        , ( "non-octal", "0o8", Nothing )
        , ( "start with underscore", "0o_1", Nothing )
        , ( "trailing underscore", "0o1_", Nothing )
        ]


hexIntValue : Test
hexIntValue =
    makeValueTests "hexadecimal integer values"
        Toml.Int
        [ ( "0", "0x0", Just 0 )
        , ( "1", "0x1", Just 1 )
        , ( "7", "0x7", Just 7 )
        , ( "longer", "0x01234567", Just 19088743 )
        , ( "longer with underscores", "0x10_4_7", Just 4167 )
        , ( "deadbeef", "0xdeadbeef", Just 3735928559 )
        , ( "dead_BEEF", "0xdead_BEEF", Just 3735928559 )
        , ( "DEADBEEF", "0xDEADBEEF", Just 3735928559 )
        , ( "empty", "0x", Nothing )
        , ( "non-hex", "0xg", Nothing )
        , ( "start with underscore", "0x_1", Nothing )
        , ( "trailing underscore", "0x1_", Nothing )
        ]


literalIntValue : Test
literalIntValue =
    makeValueTests "literal integer values"
        Toml.Int
        [ ( "0", "0", Just 0 )
        , ( "+0", "+0", Just 0 )
        , ( "-0", "-0", Just 0 )
        , ( "123", "123", Just 123 )
        , ( "-9999909", "-9999909", Just -9999909 )
        , ( "underscores allowed", "123_456", Just 123456 )
        , ( "no leading 0", "012", Nothing )
        , ( "no double sign", "--12", Nothing )
        , ( "no leading underscore", "_12", Nothing )
        , ( "no trailing underscore", "12_", Nothing )
        ]


floatVal : Test
floatVal =
    makeValueTests "float values"
        Toml.Float
        [ ( "0.0", "0.0", Just 0 )
        , ( "-0.0", "-0.0", Just 0 )
        , ( "+0.0", "+0.0", Just 0 )
        , ( "-123.456", "-123.456", Just -123.456 )
        , ( "0.001", "0.001", Just 0.001 )
        , ( "underscores", "9_224_617.445_991_228_313", Just 9224617.445991227 )
        , ( "exponentiated", "10e2", Just 1000 )
        , ( "negative exponent", "10e-2", Just 0.1 )
        , ( "negative fractional with exponent", "-1.4e-4", Just -0.00014 )
        ]


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


arrayValues : Test
arrayValues =
    makeValueTests "array values"
        Toml.Array
        [ ( "empty array"
          , "[]"
          , Just Toml.AEmpty
          )
        , ( "array of bools"
          , "[true, false, false]"
          , Just (Toml.ABool (Array.fromList [ True, False, False ]))
          )
        , ( "array of ints"
          , "[1, 2, 3]"
          , Just (Toml.AInt (Array.fromList [ 1, 2, 3 ]))
          )
        , ( "no mixed arrays"
          , "[true, 1]"
          , Nothing
          )
        , ( "whitespace between elements"
          , """[
   "foo",
   "bar" ,
   "baz"
]"""
          , Just (Toml.AString (Array.fromList [ "foo", "bar", "baz" ]))
          )
        , ( "array of arrays"
          , "[[], []]"
          , Just (Toml.AArray (Array.fromList [ Toml.AEmpty, Toml.AEmpty ]))
          )
        , ( "mixed array of arrays"
          , "[[], [true], [1, 2, 3]]"
          , Just
                (Toml.AArray
                    (Array.fromList
                        [ Toml.AEmpty
                        , Toml.ABool (Array.fromList [ True ])
                        , Toml.AInt (Array.fromList [ 1, 2, 3 ])
                        ]
                    )
                )
          )
        , ( "Trailing comma"
          , "[1, 2, ]"
          , Just (Toml.AInt (Array.fromList [ 1, 2 ]))
          )
        , ( "Trailing comma with newline"
          , """[
  true,
]"""
          , Just (Toml.ABool (Array.fromList [ True ]))
          )
        , ( "array of datetimes"
          , """[1987-07-05T17:45:00Z] """
          , Just
                (Toml.ADateTime
                    (Array.fromList
                        [ { date = { year = 1987, month = 7, day = 5 }
                          , time = { hours = 17, minutes = 45, seconds = 0 }
                          , offset = { hours = 0, minutes = 0 }
                          }
                        ]
                    )
                )
          )
        , ( "array of tables"
          , "[{ \"a\" = 1 }, { \"b\" = 2 }]"
          , Just
                (Toml.ATable
                    (Array.fromList
                        [ doc [ ( "a", Toml.Int 1 ) ]
                        , doc [ ( "b", Toml.Int 2 ) ]
                        ]
                    )
                )
          )
        ]


localDateVal : Test
localDateVal =
    [ ( "simple test"
      , "2018-08-08"
      , Just (Toml.LocalDate { year = 2018, month = 8, day = 8 })
      )
    , ( "3 digit year"
      , "200-08-09"
      , Nothing
      )
    , ( "single digit month"
      , "2018-8-08"
      , Nothing
      )
    , ( "single digit day"
      , "2018-08-8"
      , Nothing
      )
    ]
        |> List.map makeValueTest
        |> describe "local date values"


localTimeVal : Test
localTimeVal =
    [ ( "no fractional part for seconds"
      , "08:49:35"
      , Just (Toml.LocalTime { hours = 8, minutes = 49, seconds = 35 })
      )
    , ( "fractional seconds"
      , "23:59:59.999"
      , Just (Toml.LocalTime { hours = 23, minutes = 59, seconds = 59.999 })
      )
    , ( "single digits"
      , "1:1:1"
      , Nothing
      )
    ]
        |> List.map makeValueTest
        |> describe "local time values"


localDateTime : Test
localDateTime =
    [ ( "simple test"
      , "2018-08-08T08:55:12.5"
      , Just
            (Toml.LocalDateTime
                { date = { year = 2018, month = 8, day = 8 }
                , time = { hours = 8, minutes = 55, seconds = 12.5 }
                }
            )
      )
    ]
        |> List.map makeValueTest
        |> describe "local datetime values"


dateTimeVal : Test
dateTimeVal =
    [ ( "simple test"
      , "2018-08-08T10:54:00+02:00"
      , Just
            (Toml.DateTime
                { date = { year = 2018, month = 8, day = 8 }
                , time = { hours = 10, minutes = 54, seconds = 0 }
                , offset = { hours = 2, minutes = 0 }
                }
            )
      )
    ]
        |> List.map makeValueTest
        |> describe "datetime values"


structural : Test
structural =
    describe "structural checks"
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


tables : Test
tables =
    [ ( "simple table"
      , """
[foo]
bar = true
      """
      , Just
            [ ( "foo"
              , table [ ( "bar", Toml.Bool True ) ]
              )
            ]
      )
    ]
        |> List.map makeTest
        |> describe "table tests"


inlineTableVal : Test
inlineTableVal =
    makeValueTests "inline table values"
        table
        [ ( "empty"
          , "{}"
          , Just []
          )
        , ( "single pair"
          , "{ foo = true }"
          , Just [ ( "foo", Toml.Bool True ) ]
          )
        , ( "multiple pairs"
          , "{foo = 1, bar = \"hello\"}"
          , Just
                [ ( "foo", Toml.Int 1 )
                , ( "bar", Toml.String "hello" )
                ]
          )
        ]


arrayOfTables : Test
arrayOfTables =
    [ ( "single entry"
      , """
[[a]]
b = true
     """
      , Just [ ( "a", Toml.Array (Toml.ATable (Array.fromList [ doc [ ( "b", Toml.Bool True ) ] ])) ) ]
      )
    ]
        |> List.map makeTest
        |> describe "array of tables tests"


stringValues : Test
stringValues =
    makeValueTests "string value tests"
        Toml.String
        [ ( "unicode escape"
          , "\"\\u03B4\""
          , Just "δ"
          )
        , ( "long unicode escape"
          , "\"\\U000003B4\""
          , Just "δ"
          )
        ]



-- helpers


makeValueTests : String -> (a -> Toml.Value) -> List ( String, String, Maybe a ) -> Test
makeValueTests description tagger tests =
    List.map (\( n, i, r ) -> makeValueTest ( n, i, Maybe.map tagger r )) tests
        |> describe description


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
