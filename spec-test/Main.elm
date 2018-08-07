port module Main exposing (main)

import Array.Hamt as Array
import Dict
import Json.Decode exposing (Value)
import Json.Encode as Encode
import Toml
import Toml.Parser


port fromJS : (String -> msg) -> Sub msg


port toJS : Value -> Cmd msg


update : String -> model -> ( model, Cmd String )
update tomlString model =
    case Toml.Parser.parse tomlString of
        Ok tomlDoc ->
            ( model
            , encode tomlDoc |> toJS
            )

        Err _ ->
            ( model
            , toJS Encode.null
            )


main : Program Never () String
main =
    Platform.program
        { init = ( (), Cmd.none )
        , update = update
        , subscriptions = \_ -> fromJS identity
        }


encode : Toml.Document -> Value
encode doc =
    doc
        |> Dict.toList
        |> List.map (Tuple.mapSecond encodeValue)
        |> Encode.object


encodeValue : Toml.Value -> Value
encodeValue value =
    case value of
        Toml.String s ->
            val "string" s

        Toml.Int i ->
            val "integer" (toString i)

        Toml.Float f ->
            val "float" (toString f)

        Toml.Bool b ->
            val "bool" (boolToString b)

        Toml.Array arr ->
            encodeArr arr

        Toml.Table t ->
            encode t


encodeArr : Toml.ArrayValue -> Value
encodeArr arr =
    case arr of
        Toml.AString vs ->
            Array.toList vs
                |> List.map (val "string")
                |> asArray

        Toml.ABool bs ->
            Array.toList bs
                |> List.map (val "bool" << boolToString)
                |> asArray

        Toml.AInt is ->
            Array.toList is
                |> List.map (val "integer" << toString)
                |> asArray

        Toml.AFloat fs ->
            Array.toList fs
                |> List.map (val "float" << toString)
                |> asArray

        Toml.ATable ts ->
            Array.toList ts
                |> List.map encode
                |> asArray

        Toml.AArray ars ->
            Array.toList ars
                |> List.map encodeArr
                |> asArray

        Toml.AEmpty ->
            asArray []


asArray : List Value -> Value
asArray v =
    Encode.object
        [ ( "type", Encode.string "array" )
        , ( "value", Encode.list v )
        ]


val : String -> String -> Value
val key value =
    Encode.object
        [ ( "type", Encode.string key )
        , ( "value", Encode.string value )
        ]


boolToString : Bool -> String
boolToString b =
    if b then
        "true"
    else
        "false"
