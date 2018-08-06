module Example exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Test exposing (..)
import Toml
import Toml.Parser exposing (parse)


suite : Test
suite =
    describe "basic tests"
        [ test "empty document" <|
            \_ ->
                parse ""
                    |> succeed []
        , test "single key" <|
            \_ ->
                parse "key = 'val'"
                    |> succeed [ ( "key", Toml.String "val" ) ]
        , test "2 key pairs" <|
            \_ ->
                """
key1 = 'val1'
key2 = 'val2'"""
                    |> parse
                    |> succeed
                        [ ( "key1", Toml.String "val1" )
                        , ( "key2", Toml.String "val2" )
                        ]
        , test "skips empty lines" <|
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
        , test "skips comment lines" <|
            \_ ->
                "# this is a comment"
                    |> parse
                    |> succeed []
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
        , test "skips comment at end of line" <|
            \_ ->
                "key = 'val'#some comment"
                    |> parse
                    |> succeed [ ( "key", Toml.String "val" ) ]
        , test "parses bool - true" <|
            \_ ->
                "key = true"
                    |> parse
                    |> succeed [ ( "key", Toml.Bool True ) ]
        , test "parses bool - false" <|
            \_ ->
                "key = false"
                    |> parse
                    |> succeed [ ( "key", Toml.Bool False ) ]
        , test "dotted bare key" <|
            \_ ->
                "parent.child = 'nested'"
                    |> parse
                    |> succeed
                        [ ( "parent"
                          , table [ ( "child", Toml.String "nested" ) ]
                          )
                        ]
        , test "Fail: duplicate key" <|
            \_ ->
                """
key = 'val'
key = 'otherval'
      """
                    |> parse
                    |> fail
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


keys : Test
keys =
    describe "different key formats"
        [ test "bare key" <|
            \_ ->
                parse "key = ''"
                    |> succeed [ ( "key", Toml.String "" ) ]
        , test "literal key" <|
            \_ ->
                parse "'key 123' = ''"
                    |> succeed [ ( "key 123", Toml.String "" ) ]
        , test "regular key" <|
            \_ ->
                parse "\"key\" = ''"
                    |> succeed [ ( "key", Toml.String "" ) ]
        ]


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
