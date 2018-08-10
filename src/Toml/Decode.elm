module Toml.Decode
    exposing
        ( DecodeError(..)
        , Decoder
        , Error(..)
        , Errors
        , andMap
        , at
        , bool
        , dateTime
        , decodeDocument
        , decodeString
        , dict
        , fail
        , field
        , float
        , index
        , int
        , list
        , localDate
        , localDateTime
        , map
        , map2
        , mapError
        , string
        , succeed
        )

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Parser
import Toml
import Toml.Calendar as Calendar
import Toml.Parser


type Error e
    = ParseError Parser.Error
    | DecodeErrors (Errors e)


type DecodeError e
    = Expected String Toml.Value
    | MissingIndex Int
    | MissingField String
    | AtIndex Int (Errors e)
    | InField String (Errors e)
    | Custom e


type alias Errors e =
    ( DecodeError e, List (DecodeError e) )


type Decoder e a
    = Decoder (Toml.Value -> Result (Errors e) a)


decodeString : Decoder e a -> String -> Result (Error e) a
decodeString decoder input =
    Toml.Parser.parse input
        |> Result.mapError ParseError
        |> Result.andThen
            (decodeDocument decoder
                >> Result.mapError DecodeErrors
            )


decodeDocument : Decoder e a -> Toml.Document -> Result (Errors e) a
decodeDocument (Decoder decoderFn) doc =
    decoderFn (Toml.Table doc)


succeed : a -> Decoder e a
succeed v =
    Decoder <| \_ -> Ok v


fail : e -> Decoder e v
fail e =
    Decoder <| \_ -> Err ( Custom e, [] )


map : (a -> b) -> Decoder e a -> Decoder e b
map f (Decoder decoderFn) =
    Decoder (Result.map f << decoderFn)


mapError : (e1 -> e2) -> Decoder e1 a -> Decoder e2 a
mapError f (Decoder decoderFn) =
    Decoder (Result.mapError (mapErrors f) << decoderFn)


mapErrors : (e1 -> e2) -> Errors e1 -> Errors e2
mapErrors f ( e, es ) =
    ( mapCustom f e, List.map (mapCustom f) es )


mapCustom : (e1 -> e2) -> DecodeError e1 -> DecodeError e2
mapCustom f error =
    case error of
        Expected e v ->
            Expected e v

        MissingField k ->
            MissingField k

        MissingIndex i ->
            MissingIndex i

        Custom e ->
            Custom (f e)

        AtIndex idx e ->
            AtIndex idx (mapErrors f e)

        InField field e ->
            InField field (mapErrors f e)


map2 : (a -> b -> c) -> Decoder e a -> Decoder e b -> Decoder e c
map2 f (Decoder decA) (Decoder decB) =
    Decoder <|
        \v ->
            case ( decA v, decB v ) of
                ( Ok a, Ok b ) ->
                    Ok (f a b)

                ( Err ( l, ls ), Err ( r, rs ) ) ->
                    Err ( l, ls ++ r :: rs )

                ( Err e, _ ) ->
                    Err e

                ( _, Err e ) ->
                    Err e


andMap : Decoder e a -> Decoder e (a -> b) -> Decoder e b
andMap second first =
    map2 (<|) first second


string : Decoder e String
string =
    Decoder <|
        \v ->
            case v of
                Toml.String s ->
                    Ok s

                _ ->
                    expected "string" v


int : Decoder e Int
int =
    Decoder <|
        \v ->
            case v of
                Toml.Int i ->
                    Ok i

                _ ->
                    expected "int" v


float : Decoder e Float
float =
    Decoder <|
        \v ->
            case v of
                Toml.Float f ->
                    Ok f

                _ ->
                    expected "float" v


bool : Decoder e Bool
bool =
    Decoder <|
        \v ->
            case v of
                Toml.Bool b ->
                    Ok b

                _ ->
                    expected "bool" v


localDate : Decoder e Calendar.Date
localDate =
    Decoder <|
        \v ->
            case v of
                Toml.LocalDate ld ->
                    Ok ld

                _ ->
                    expected "localDate" v


localTime : Decoder e Calendar.Time
localTime =
    Decoder <|
        \v ->
            case v of
                Toml.LocalTime lt ->
                    Ok lt

                _ ->
                    expected "localTime" v


localDateTime : Decoder e Calendar.LocalDateTime
localDateTime =
    Decoder <|
        \v ->
            case v of
                Toml.LocalDateTime ldt ->
                    Ok ldt

                _ ->
                    expected "localDateTime" v


dateTime : Decoder e Calendar.DateTime
dateTime =
    Decoder <|
        \v ->
            case v of
                Toml.DateTime dt ->
                    Ok dt

                _ ->
                    expected "dateTime" v


field : String -> Decoder e a -> Decoder e a
field key (Decoder decoderFn) =
    Decoder <|
        \v ->
            case v of
                Toml.Table t ->
                    case Dict.get key t of
                        Just v ->
                            decoderFn v
                                |> Result.mapError (\e -> ( InField key e, [] ))

                        Nothing ->
                            Err ( MissingField key, [] )

                _ ->
                    expected "table" v


dict : Decoder e a -> Decoder e (Dict String a)
dict (Decoder fieldDecoder) =
    Decoder <|
        \v ->
            case v of
                Toml.Table t ->
                    Dict.foldr
                        (\key value -> collectDict key (fieldDecoder value))
                        (Ok Dict.empty)
                        t

                _ ->
                    expected "table" v


collectDict :
    String
    -> Result (Errors e) a
    -> Result (Errors e) (Dict String a)
    -> Result (Errors e) (Dict String a)
collectDict key res acc =
    case ( res, acc ) of
        ( Err e, Err ( er, ers ) ) ->
            Err ( InField key e, er :: ers )

        ( Err e, Ok _ ) ->
            Err ( InField key e, [] )

        ( Ok _, Err e ) ->
            acc

        ( Ok v, Ok vs ) ->
            Ok (Dict.insert key v vs)


at : List String -> Decoder e a -> Decoder e a
at fields dec =
    List.foldr field dec fields


list : Decoder e a -> Decoder e (List a)
list entryDecoder =
    Decoder <|
        \v ->
            case v of
                Toml.Array Toml.AEmpty ->
                    Ok []

                Toml.Array (Toml.AString s) ->
                    listHelper Toml.String s entryDecoder

                Toml.Array (Toml.ABool b) ->
                    listHelper Toml.Bool b entryDecoder

                Toml.Array (Toml.AInt i) ->
                    listHelper Toml.Int i entryDecoder

                Toml.Array (Toml.AFloat f) ->
                    listHelper Toml.Float f entryDecoder

                Toml.Array (Toml.ALocalDate ld) ->
                    listHelper Toml.LocalDate ld entryDecoder

                Toml.Array (Toml.ALocalTime lt) ->
                    listHelper Toml.LocalTime lt entryDecoder

                Toml.Array (Toml.ALocalDateTime ldt) ->
                    listHelper Toml.LocalDateTime ldt entryDecoder

                Toml.Array (Toml.ADateTime dt) ->
                    listHelper Toml.DateTime dt entryDecoder

                Toml.Array (Toml.ATable t) ->
                    listHelper Toml.Table t entryDecoder

                Toml.Array (Toml.AArray a) ->
                    listHelper Toml.Array a entryDecoder

                _ ->
                    expected "array" v


index : Int -> Decoder e a -> Decoder e a
index idx entryDecoder =
    Decoder <|
        \v ->
            case v of
                Toml.Array arr ->
                    indexArr idx entryDecoder arr

                _ ->
                    expected "array" v


indexArr : Int -> Decoder e a -> Toml.ArrayValue -> Result (Errors e) a
indexArr idx entryDecoder v =
    case v of
        Toml.AEmpty ->
            missingIndex idx

        Toml.AString s ->
            indexHelper idx entryDecoder Toml.String s

        Toml.ABool b ->
            indexHelper idx entryDecoder Toml.Bool b

        Toml.AInt i ->
            indexHelper idx entryDecoder Toml.Int i

        Toml.AFloat f ->
            indexHelper idx entryDecoder Toml.Float f

        Toml.ALocalDate ld ->
            indexHelper idx entryDecoder Toml.LocalDate ld

        Toml.ALocalTime lt ->
            indexHelper idx entryDecoder Toml.LocalTime lt

        Toml.ALocalDateTime ldt ->
            indexHelper idx entryDecoder Toml.LocalDateTime ldt

        Toml.ADateTime dt ->
            indexHelper idx entryDecoder Toml.DateTime dt

        Toml.ATable t ->
            indexHelper idx entryDecoder Toml.Table t

        Toml.AArray a ->
            indexHelper idx entryDecoder Toml.Array a


indexHelper :
    Int
    -> Decoder e a
    -> (v -> Toml.Value)
    -> Array v
    -> Result (Errors e) a
indexHelper idx (Decoder entryDecoderFn) toTomlValue arr =
    case Array.get idx arr of
        Just v ->
            entryDecoderFn (toTomlValue v)
                |> Result.mapError (\e -> ( AtIndex idx e, [] ))

        Nothing ->
            missingIndex idx


listHelper :
    (v -> Toml.Value)
    -> Array v
    -> Decoder e a
    -> Result (Errors e) (List a)
listHelper toTomlVal vals (Decoder decoderFn) =
    Array.foldr (collectArr << decoderFn << toTomlVal)
        ( Array.length vals - 1, Ok [] )
        vals
        |> Tuple.second


collectArr :
    Result (Errors e) a
    -> ( Int, Result (Errors e) (List a) )
    -> ( Int, Result (Errors e) (List a) )
collectArr res ( idx, acc ) =
    case ( res, acc ) of
        ( Err e, Err ( er, ers ) ) ->
            ( idx - 1, Err ( AtIndex idx e, er :: ers ) )

        ( Err e, Ok _ ) ->
            ( idx - 1, Err ( AtIndex idx e, [] ) )

        ( Ok _, Err e ) ->
            ( idx - 1, acc )

        ( Ok v, Ok vs ) ->
            ( idx - 1, Ok (v :: vs) )



-- Helpers


expected : String -> Toml.Value -> Result (Errors e) a
expected expectedType actualValue =
    Err ( Expected expectedType actualValue, [] )


missingIndex : Int -> Result (Errors e) a
missingIndex idx =
    Err ( MissingIndex idx, [] )