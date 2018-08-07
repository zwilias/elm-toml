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
      , "key = true"
      , Just [ ( "key", Toml.Bool True ) ]
      )
    , ( "parse false"
      , "key = false"
      , Just [ ( "key", Toml.Bool False ) ]
      )
    , ( "case sensitive"
      , "key = True"
      , Nothing
      )
    ]
        |> List.map makeTest
        |> describe "boolean values"


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
