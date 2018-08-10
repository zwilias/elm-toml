module Toml.Decode.Tests exposing (..)

import Expect exposing (Expectation)
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
        ]



-- helpers


t : ( String, String, Decoder e a, Result (Errors e) a ) -> Test
t ( name, input, decoder, result ) =
    test name <|
        \_ ->
            D.decodeString decoder input
                |> Expect.equal (Result.mapError D.DecodeErrors result)
