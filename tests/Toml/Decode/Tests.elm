module Toml.Decode.Tests exposing (..)

import Expect
import Test exposing (..)
import Toml
import Toml.Decode as D exposing (Decoder, Error, Errors)


basic : Test
basic =
    describe "basic tests"
        [ t
            ( "first!"
            , """
key = "value"
              """
            , D.field "key" D.string
            , Ok "value"
            )
        , t
            ( "list of strings"
            , """
# I'm pretty proud of this!
key = ['foo', 'bar', 'baz']
              """
            , D.field "key" (D.list D.string)
            , Ok [ "foo", "bar", "baz" ]
            )
        , t
            ( "int as string error"
            , """
key = 5
              """
            , D.field "key" D.string
            , Err ( D.InField "key" ( D.Expected "string" (Toml.Int 5), [] ), [] )
            )
        , t ( "decodes ints", "key = 5", D.field "key" D.int, Ok 5 )
        , t
            ( "only ints as int"
            , "key = 0.1"
            , D.field "key" D.int
            , Err ( D.InField "key" ( D.Expected "int" (Toml.Float 0.1), [] ), [] )
            )
        , t ( "decodes bools", "key = true", D.field "key" D.bool, Ok True )
        , t
            ( "only bools as bool"
            , "key = 0.1"
            , D.field "key" D.bool
            , Err ( D.InField "key" ( D.Expected "bool" (Toml.Float 0.1), [] ), [] )
            )
        , t ( "decodes floats", "key = 0.1", D.field "key" D.float, Ok 0.1 )
        , t
            ( "only floats as float"
            , "key = 0"
            , D.field "key" D.float
            , Err ( D.InField "key" ( D.Expected "float" (Toml.Int 0), [] ), [] )
            )
        , t ( "Empty arr", "key = []", D.field "key" (D.list D.int), Ok [] )
        , t ( "List of bools", "key = [true, false]", D.field "key" (D.list D.bool), Ok [ True, False ] )
        , t ( "List of ints", "key = [-1, 0, 1]", D.field "key" (D.list D.int), Ok [ -1, 0, 1 ] )
        , t ( "List of floats", "key = [-0.2, 0.0, 5.4]", D.field "key" (D.list D.float), Ok [ -0.2, 0, 5.4 ] )
        , t ( "List of strings", "key = ['foo', 'bar']", D.field "key" (D.list D.string), Ok [ "foo", "bar" ] )
        ]


combining : Test
combining =
    describe "combinating decoders"
        [ t
            ( "make pair"
            , """
one = "foo"
two = 123
              """
            , D.map2 (,) (D.field "one" D.string) (D.field "two" D.int)
            , Ok ( "foo", 123 )
            )
        , t
            ( "first error"
            , """
one = 123
two = 123
              """
            , D.map2 (,) (D.field "one" D.string) (D.field "two" D.int)
            , Err ( D.InField "one" ( D.Expected "string" (Toml.Int 123), [] ), [] )
            )
        , t
            ( "second error"
            , """
one = "foo"
two = "bar"
              """
            , D.map2 (,) (D.field "one" D.string) (D.field "two" D.int)
            , Err ( D.InField "two" ( D.Expected "int" (Toml.String "bar"), [] ), [] )
            )
        , t
            ( "both error"
            , """
one = 123
two = "bar"
              """
            , D.map2 (,) (D.field "one" D.string) (D.field "two" D.int)
            , Err
                ( D.InField "one"
                    ( D.Expected "string" (Toml.Int 123)
                    , []
                    )
                , [ D.InField "two" ( D.Expected "int" (Toml.String "bar"), [] ) ]
                )
            )
        ]


parseFailure : Test
parseFailure =
    test "Expect parse failure" <|
        \_ ->
            case D.decodeString D.string "foo" of
                Err (D.ParseError e) ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected parse error"



-- helpers


t : ( String, String, Decoder e a, Result (Errors e) a ) -> Test
t ( name, input, decoder, result ) =
    test name <|
        \_ ->
            D.decodeString decoder input
                |> Expect.equal (Result.mapError D.DecodeErrors result)
