module Toml.Decode
    exposing
        ( DecodeError(..)
        , Decoder
        , Error(..)
        , Errors
        , andMap
        , andThen
        , at
        , bool
        , dateTime
        , decodeString
        , dict
        , fail
        , field
        , float
        , index
        , int
        , lazy
        , list
        , localDate
        , localDateTime
        , localTime
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , mapError
        , oneOf
        , string
        , succeed
        )

{-| Decode a TOML string into an Elm datastructure.


# Running

@docs Decoder, decodeString


# Primitive decoders

@docs int, float, string, bool, localTime, localDate, localDateTime, dateTime


# Navigating structure

@docs field, at, dict, index, list


# Building more complex decoders

@docs succeed, fail, map, oneOf, andThen, mapError, map2, andMap, map3, map4, map5, map6, map7, map8, lazy


# Errors

@docs Error, Errors, DecodeError

-}

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Parser
import Toml
import Toml.Calendar as Calendar
import Toml.Parser


{-| Decoding a string representing a TOML document may produce errors. Those
will either be parse errors (represented by a `Parser.Error`) or error that
occurred during the actual decoding process (represented by `DecodeErrors`).
-}
type Error e
    = ParseError Parser.Error
    | DecodeErrors (Errors e)


{-| While decoding, these errors may be produced.

Type mismatches, missing field, missing indices and custom errors (from `fail`)
can occur. If those errors occur in the context of a field (through `field`,
`at`, or `dict`) they are wrapped in `InField`. If the error occurs at a certain
index in a list (through `list` or `index`), it will be wrapped in `AtIndex`.

Type mismatches also carry the original value, for inspection.

-}
type DecodeError e
    = Expected String Toml.Value
    | MissingIndex Int
    | MissingField String
    | AtIndex Int (Errors e)
    | InField String (Errors e)
    | OneOf (List (Errors e))
    | Custom e


{-| One or more `DecodeError`s.
-}
type alias Errors e =
    ( DecodeError e, List (DecodeError e) )


{-| Represents a decoder, constructed from the functions in this module, that
can be executed against a string representing a TOML document using
`decodeString`.
-}
type Decoder e a
    = Decoder (Toml.Value -> Result (Errors e) a)


{-| Run a `Decoder` against a string representing a TOML document.

    decodeString (field "foo" string) "foo = 'bar'"
    --> Ok "bar"

-}
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


{-| Construct a `Decoder` that will always succeed (provided the TOML is valid)
with the given value.

    decodeString (succeed "hello world!") "foo = 'bar'"
    --> Ok "hello world!"


    "invalid document"
        |> decodeString (succeed "nope")
        |> Result.mapError (always "failure")
    --> Err "failure"

-}
succeed : a -> Decoder e a
succeed v =
    Decoder <| \_ -> Ok v


{-| Construct a `Decoder` that will always fail (provided a valid TOML document)
with the given value.

Note that this needn't be a string: custom types are allowed, too!

    type MyError
        = MyError


    "ok = true"
        |> decodeString (fail MyError)
    --> Err (DecodeErrors (Custom MyError, []))

-}
fail : e -> Decoder e v
fail e =
    Decoder <| \_ -> Err ( Custom e, [] )


{-| Transform the value a decoder will produce.

    shout : Decoder e String
    shout =
        map String.toUpper string


    "message = 'hello there'"
        |> decodeString (field "message" shout)
    --> Ok "HELLO THERE"

-}
map : (a -> b) -> Decoder e a -> Decoder e b
map f (Decoder decoderFn) =
    Decoder (Result.map f << decoderFn)


{-| Transform the error value produced by your custom error type.

Perhaps you want to reuse decoders across multiple modules, each producing a
certain type of errors? Either way, this allows transforming them.

    type BaseError = BaseError
    type Wrapped = Wrapped BaseError


    baseDecoder : Decoder BaseError a
    baseDecoder =
        fail BaseError


    wrappedDecoder : Decoder Wrapped a
    wrappedDecoder =
        mapError Wrapped baseDecoder


    "message = 'hi'"
        |> decodeString (field "message" wrappedDecoder)
    --> Err (DecodeErrors (InField "message" (Custom (Wrapped BaseError), []), []))

-}
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

        OneOf es ->
            OneOf (List.map (mapErrors f) es)


{-| Combine the values produced by 2 decoders using a function.

    type alias Person =
        { name : String
        , age : Int
        }

    person : Decoder e Person
    person =
        map2 Person
            (field "name" string)
            (field "age" int)


    people : Decoder e (List Person)
    people =
        list person


    """
    [[people]]
    name = "Alice"
    age = 43
    [[people]]
    name = "Bob"
    age = 34
    """
        |> decodeString (field "people" people)
    --> Ok [ { name = "Alice", age = 43 }
    -->    , { name = "Bob", age = 34 }
    -->    ]

-}
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


{-| Useful to make pipelines of decoders, by decoding to a function and applying
it to values produced by other pieces of the pipeline.

    type alias Person =
        { name : String
        , age : Int
        }

    person : Decoder e Person
    person =
        -- `Decoder e (String -> Int -> Person)`
        succeed Person
            -- adding a string, so `Decoder e (Int -> Person)`
            |> andMap (field "name" string)
            -- And with the int added, we get a `Decoder e Person`
            |> andMap (field "age" int)


    people : Decoder e (List Person)
    people =
        list person


    """
    [[people]]
    name = "Alice"
    age = 43
    [[people]]
    name = "Bob"
    age = 34
    """
        |> decodeString (field "people" people)
    --> Ok [ { name = "Alice", age = 43 }
    -->    , { name = "Bob", age = 34 }
    -->    ]

-}
andMap : Decoder e a -> Decoder e (a -> b) -> Decoder e b
andMap second first =
    map2 (<|) first second


{-| Create a decoder based on the value produced by a different decoders.

This can be especially useful when decoding based on some sort of discriminating
value.

    type Utensil
        = Spoon
        | Knife { serrated : Bool }
        | Fork { prongs : Int }


    type MyError
        = UnknownUtensilType String


    knifeInfo : Decoder e { serrated : Bool }
    knifeInfo =
        map (\serrated -> { serrated = serrated })
            (field "serrated" bool)


    forkInfo : Decoder e { prongs : Int }
    forkInfo =
        map (\prongs -> { prongs = prongs })
            (field "prongs" int)


    utensilFromString : String -> Decoder String Utensil
    utensilFromString utensilType =
        case utensilType of
            "spoon" ->
                succeed Spoon
            "knife" ->
                map Knife knifeInfo
            "fork" ->
                map Fork forkInfo
            _ ->
                fail utensilType


    utensil : Decoder MyError Utensil
    utensil =
        field "variety" string
            |> andThen utensilFromString
            |> mapError UnknownUtensilType


    input : String
    input =
        """
    [[utensils]]
    variety = 'spoon'
    [[utensils]]
    variety = 'knife'
    serrated = false
    [[utensils]]
    variety = 'fork'
    prongs = 3
        """

    decodeString (field "utensils" (list utensil)) input
    --> Ok [ Spoon
    -->    , Knife { serrated = False }
    -->    , Fork { prongs = 3 }
    -->    ]

-}
andThen : (a -> Decoder e b) -> Decoder e a -> Decoder e b
andThen toDecB (Decoder decoderFn) =
    Decoder <|
        \v ->
            case decoderFn v of
                Ok a ->
                    run (toDecB a) v

                Err e ->
                    Err e


{-| Combine 3 decoders.
-}
map3 :
    (a -> b -> c -> d)
    -> Decoder e a
    -> Decoder e b
    -> Decoder e c
    -> Decoder e d
map3 f decA decB decC =
    map2 f decA decB
        |> andMap decC


{-| Combine 4 decoders.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Decoder x a
    -> Decoder x b
    -> Decoder x c
    -> Decoder x d
    -> Decoder x e
map4 f decA decB decC decD =
    map3 f decA decB decC
        |> andMap decD


{-| Combine 5 decoders.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Decoder x a
    -> Decoder x b
    -> Decoder x c
    -> Decoder x d
    -> Decoder x e
    -> Decoder x f
map5 f decA decB decC decD decE =
    map4 f decA decB decC decD
        |> andMap decE


{-| Combine 6 decoders.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Decoder x a
    -> Decoder x b
    -> Decoder x c
    -> Decoder x d
    -> Decoder x e
    -> Decoder x f
    -> Decoder x g
map6 f decA decB decC decD decE decF =
    map5 f decA decB decC decD decE
        |> andMap decF


{-| Combine 7 decoders.
-}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Decoder x a
    -> Decoder x b
    -> Decoder x c
    -> Decoder x d
    -> Decoder x e
    -> Decoder x f
    -> Decoder x g
    -> Decoder x h
map7 f decA decB decC decD decE decF decG =
    map6 f decA decB decC decD decE decF
        |> andMap decG


{-| Combine 8 decoders.
-}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> Decoder x a
    -> Decoder x b
    -> Decoder x c
    -> Decoder x d
    -> Decoder x e
    -> Decoder x f
    -> Decoder x g
    -> Decoder x h
    -> Decoder x i
map8 f decA decB decC decD decE decF decG decH =
    map7 f decA decB decC decD decE decF decG
        |> andMap decH


{-| Required for constructing recursive decoders.

    type Tree
        = Node String
        | Branch String (List Tree)


    branch : Decoder e Tree
    branch =
        map2 Branch
            (field "label" string)
            (field "children" (lazy (\_ -> list tree)))


    node : Decoder e Tree
    node =
        map Node (field "label" string)


    tree : Decoder e Tree
    tree =
        oneOf
            [ lazy (\_ -> branch)
            , node
            ]


    """
    label = 'root'
    [[children]]
    label = 'first child'
    [[children]]
    label = 'a branch'
    [[children.children]]
    label = 'a nested child'
    [[children.children]]
    label = 'another nested child'
    """
        |> decodeString tree
    --> Ok ( Branch "root"
    -->        [ Node "first child"
    -->        , Branch "a branch"
    -->            [ Node "a nested child"
    -->            , Node "another nested child"
    -->            ]
    -->        ]
    -->    )

-}
lazy : (() -> Decoder e a) -> Decoder e a
lazy dec =
    Decoder <| \v -> run (dec ()) v


{-| Try a bunch of decoders and succeed with the result of the first decoder to
succeed.

If none of the decoders succeed, the errors are collected and returned using the
`OneOf` constructor.

    type StringOrInt = AString String | AnInt Int


    stringOrInt : Decoder e StringOrInt
    stringOrInt =
        oneOf
            [ map AString string
            , map AnInt int
            ]


    input : String
    input =
        """
    item1 = "foo"
    item2 = 123
    item3 = "bar"
        """


    decodeString (field "item1" stringOrInt) input
    --> Ok (AString "foo")

    decodeString (field "item2" stringOrInt) input
    --> Ok (AnInt 123)

    decodeString (field "item3" stringOrInt) input
    --> Ok (AString "bar")

-}
oneOf : List (Decoder e a) -> Decoder e a
oneOf decoders =
    Decoder <| \v -> oneOfHelp v decoders []


oneOfHelp :
    Toml.Value
    -> List (Decoder e a)
    -> List (Errors e)
    -> Result (Errors e) a
oneOfHelp val decoders acc =
    case decoders of
        [] ->
            Err ( OneOf (List.reverse acc), [] )

        decoder :: rest ->
            case run decoder val of
                Ok v ->
                    Ok v

                Err e ->
                    oneOfHelp val rest (e :: acc)


{-| Decode a string value.

    "my-key = 'my value'"
        |> decodeString (field "my-key" string)
    --> Ok "my value"

-}
string : Decoder e String
string =
    Decoder <|
        \v ->
            case v of
                Toml.String s ->
                    Ok s

                _ ->
                    expected "string" v


{-| Decode an integer value.

    "my-key = -703"
        |> decodeString (field "my-key" int)
    --> Ok -703

-}
int : Decoder e Int
int =
    Decoder <|
        \v ->
            case v of
                Toml.Int i ->
                    Ok i

                _ ->
                    expected "int" v


{-| Decode a floating point value.

    "my-key = 123.456"
        |> decodeString (field "my-key" float)
    --> Ok 123.456

-}
float : Decoder e Float
float =
    Decoder <|
        \v ->
            case v of
                Toml.Float f ->
                    Ok f

                _ ->
                    expected "float" v


{-| Decode a boolean value.

    "my-key = true"
        |> decodeString (field "my-key" bool)
    --> Ok True

-}
bool : Decoder e Bool
bool =
    Decoder <|
        \v ->
            case v of
                Toml.Bool b ->
                    Ok b

                _ ->
                    expected "bool" v


{-| Decode a local date.

    "today = 2018-08-11"
        |> decodeString (field "today" localDate)
    --> Ok { year = 2018, month = 8, day = 11 }

-}
localDate : Decoder e Calendar.Date
localDate =
    Decoder <|
        \v ->
            case v of
                Toml.LocalDate ld ->
                    Ok ld

                _ ->
                    expected "localDate" v


{-| Decode a local time.

    "now = 14:08:49.02"
        |> decodeString (field "now" localTime)
    --> Ok { hours = 14, minutes = 8, seconds = 49.02 }

-}
localTime : Decoder e Calendar.Time
localTime =
    Decoder <|
        \v ->
            case v of
                Toml.LocalTime lt ->
                    Ok lt

                _ ->
                    expected "localTime" v


{-| Decode a local date-time.

    "now = 2018-08-11T14:10:23.70"
        |> decodeString (field "now" localDateTime)
    --> Ok
    -->   { date = { year = 2018, month = 8, day = 11 }
    -->   , time = { hours = 14, minutes = 10, seconds = 23.70 }
    -->   }

-}
localDateTime : Decoder e Calendar.LocalDateTime
localDateTime =
    Decoder <|
        \v ->
            case v of
                Toml.LocalDateTime ldt ->
                    Ok ldt

                _ ->
                    expected "localDateTime" v


{-| Decode an absolute time with UTF offset.

    "now = 2018-08-11T14:10:23.70Z"
        |> decodeString (field "now" dateTime)
    --> Ok
    -->   { date = { year = 2018, month = 8, day = 11 }
    -->   , time = { hours = 14, minutes = 10, seconds = 23.70 }
    -->   , offset = { hours = 0, minutes = 0 }
    -->   }

-}
dateTime : Decoder e Calendar.DateTime
dateTime =
    Decoder <|
        \v ->
            case v of
                Toml.DateTime dt ->
                    Ok dt

                _ ->
                    expected "dateTime" v


{-| TODO
-}
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


{-| TODO
-}
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


{-| TODO
-}
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


{-| TODO
-}
at : List String -> Decoder e a -> Decoder e a
at fields dec =
    List.foldr field dec fields


{-| TODO
-}
list : Decoder e a -> Decoder e (List a)
list entryDecoder =
    Decoder <|
        \v ->
            case v of
                Toml.Array arrayValue ->
                    listValue arrayValue entryDecoder

                _ ->
                    expected "array" v


listValue : Toml.ArrayValue -> Decoder e a -> Result (Errors e) (List a)
listValue v entryDecoder =
    case v of
        Toml.AEmpty ->
            Ok []

        Toml.AString s ->
            listHelper Toml.String s entryDecoder

        Toml.ABool b ->
            listHelper Toml.Bool b entryDecoder

        Toml.AInt i ->
            listHelper Toml.Int i entryDecoder

        Toml.AFloat f ->
            listHelper Toml.Float f entryDecoder

        Toml.ALocalDate ld ->
            listHelper Toml.LocalDate ld entryDecoder

        Toml.ALocalTime lt ->
            listHelper Toml.LocalTime lt entryDecoder

        Toml.ALocalDateTime ldt ->
            listHelper Toml.LocalDateTime ldt entryDecoder

        Toml.ADateTime dt ->
            listHelper Toml.DateTime dt entryDecoder

        Toml.ATable t ->
            listHelper Toml.Table t entryDecoder

        Toml.AArray a ->
            listHelper Toml.Array a entryDecoder


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


{-| TODO
-}
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



-- Helpers


expected : String -> Toml.Value -> Result (Errors e) a
expected expectedType actualValue =
    Err ( Expected expectedType actualValue, [] )


missingIndex : Int -> Result (Errors e) a
missingIndex idx =
    Err ( MissingIndex idx, [] )


run : Decoder e a -> Toml.Value -> Result (Errors e) a
run (Decoder decoderFn) =
    decoderFn
