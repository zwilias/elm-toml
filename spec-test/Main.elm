port module Main exposing (main)

import Array.Hamt as Array
import Dict
import Json.Decode exposing (Value)
import Json.Encode as Encode
import Toml
import Toml.Calendar as Calendar
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

        Toml.LocalDate d ->
            val "local-date" (dateToString d)

        Toml.LocalTime t ->
            val "local-time" (timeToString t)

        Toml.LocalDateTime dt ->
            val "local-datetime" (dateToString dt.date ++ "T" ++ timeToString dt.time)

        Toml.DateTime dt ->
            val "datetime" (dateTimeToString dt)

        Toml.Array arr ->
            encodeArr arr

        Toml.Table t ->
            encode t


dateToString : Calendar.Date -> String
dateToString { year, month, day } =
    toPaddedString 4 year
        ++ "-"
        ++ toPaddedString 2 month
        ++ "-"
        ++ toPaddedString 2 day


timeToString : Calendar.Time -> String
timeToString { hours, minutes, seconds } =
    toPaddedString 2 hours
        ++ ":"
        ++ toPaddedString 2 minutes
        ++ ":"
        ++ secondsToString seconds


secondsToString : Float -> String
secondsToString s =
    if toFloat (round s) == s then
        toPaddedString 2 (round s)
    else
        toPaddedString 2 (floor s)
            ++ "."
            ++ toString (s - toFloat (floor s))


dateTimeToString : Calendar.DateTime -> String
dateTimeToString { date, time, offset } =
    dateToString date
        ++ "T"
        ++ timeToString time
        ++ offsetToString offset


offsetToString : Calendar.Offset -> String
offsetToString { hours, minutes } =
    if hours == 0 && minutes == 0 then
        "Z"
    else if hours > 0 then
        toPaddedString 2 hours
            ++ ":"
            ++ toPaddedString 2 minutes
    else
        "-"
            ++ toPaddedString 2 (abs hours)
            ++ ":"
            ++ toPaddedString 2 minutes


toPaddedString : Int -> Int -> String
toPaddedString l v =
    toString v
        |> String.padLeft l '0'


localDateTimeToString : Calendar.LocalDateTime -> String
localDateTimeToString dt =
    dateToString dt.date ++ "T" ++ timeToString dt.time


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

        Toml.ALocalDate ds ->
            Array.toList ds
                |> List.map (val "local-date" << dateToString)
                |> asArray

        Toml.ALocalTime ts ->
            Array.toList ts
                |> List.map (val "local-time" << timeToString)
                |> asArray

        Toml.ALocalDateTime dts ->
            Array.toList dts
                |> List.map (val "local-datetime" << localDateTimeToString)
                |> asArray

        Toml.ADateTime dts ->
            Array.toList dts
                |> List.map (val "datetime" << dateTimeToString)
                |> asArray

        Toml.ATable ts ->
            Array.toList ts
                |> List.map encode
                |> Encode.list

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
