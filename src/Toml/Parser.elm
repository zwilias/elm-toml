module Toml.Parser exposing (parse)

import Array.Hamt as Array exposing (Array)
import Char
import Dict exposing (Dict)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Count(..)
        , Parser
        , andThen
        , delayedCommit
        , delayedCommitMap
        , end
        , fail
        , ignore
        , ignoreUntil
        , inContext
        , keep
        , keyword
        , lazy
        , map
        , oneOf
        , oneOrMore
        , repeat
        , source
        , succeed
        , symbol
        , zeroOrMore
        )
import String.UTF32
import Toml
import Toml.Calendar as Calendar


{-| Run the parser and extract the final result
-}
parse : String -> Result Parser.Error Toml.Document
parse input =
    Parser.run document input



-- Auxilliary types


type alias Key =
    ( String, List String )


type alias Acc =
    { final : Toml.Document
    , current : Toml.Document
    , context : List String
    , isArray : Bool
    }


type Entry
    = Empty String
    | KVPair ( Key, Toml.Value )
    | Header Bool Key


emptyAcc : Acc
emptyAcc =
    { final = Dict.empty
    , current = Dict.empty
    , context = []
    , isArray = False
    }


freshAcc : Key -> Bool -> Toml.Document -> Acc
freshAcc ( x, xs ) isArray doc =
    { final = doc
    , current = Dict.empty
    , context = x :: xs
    , isArray = isArray
    }



-- Parsing the document


document : Parser Toml.Document
document =
    accumulateDoc emptyAcc
        |> andThen finalize


accumulateDoc : Acc -> Parser Acc
accumulateDoc acc =
    succeed identity
        |. ws
        |= parseEntry
        |> andThen (commit acc)
        |> andThen
            (\newAcc ->
                oneOf
                    [ end |> map (always newAcc)
                    , lazy (\_ -> accumulateDoc newAcc)
                    ]
            )


commit : Acc -> Entry -> Parser Acc
commit acc entry =
    case entry of
        Empty _ ->
            succeed acc

        KVPair ( key, val ) ->
            case addKVPair key val acc.current of
                Ok newDoc ->
                    succeed { acc | current = newDoc }

                Err e ->
                    fail e

        Header isArray key ->
            finalize acc
                |> Parser.map (freshAcc key isArray)


addKVPair : Key -> Toml.Value -> Toml.Document -> Result String Toml.Document
addKVPair ( key, rest ) val doc =
    case rest of
        [] ->
            if existingKey key doc then
                Err <| "Duplicate key: `" ++ key ++ "`"
            else
                Ok <| Dict.insert key val doc

        x :: xs ->
            find key doc
                |> Result.andThen (addKVPair ( x, xs ) val)
                |> Result.map (saveAsKey key doc)


saveAsKey : String -> Toml.Document -> Toml.Document -> Toml.Document
saveAsKey key docToSaveIn docToSave =
    Dict.insert key (Toml.Table docToSave) docToSaveIn


find : String -> Toml.Document -> Result String Toml.Document
find key doc =
    case Dict.get key doc of
        Just (Toml.Table t) ->
            Ok t

        Just _ ->
            Err <| "Key is not a table: `" ++ key ++ "`"

        Nothing ->
            Ok Dict.empty


existingKey : String -> Toml.Document -> Bool
existingKey key doc =
    Dict.member key doc


finalize : Acc -> Parser Toml.Document
finalize acc =
    case acc.context of
        [] ->
            succeed (Dict.union acc.final acc.current)

        k :: rest ->
            if acc.isArray then
                appendDoc ( k, rest ) acc.current acc.final
                    |> result fail succeed
            else
                mergeDoc ( k, rest ) acc.current acc.final
                    |> result fail succeed


mergeDoc : Key -> Toml.Document -> Toml.Document -> Result String Toml.Document
mergeDoc ( k, rest ) docToAdd container =
    case rest of
        [] ->
            case Dict.get k container of
                Just (Toml.Table t) ->
                    Dict.insert k (Toml.Table (Dict.union t docToAdd)) container
                        |> Ok

                Just _ ->
                    Err "merging into a non-table"

                Nothing ->
                    Ok <| Dict.insert k (Toml.Table docToAdd) container

        x :: rest ->
            case Dict.get k container of
                Just (Toml.Table t) ->
                    mergeDoc ( x, rest ) docToAdd t
                        |> Result.map (\newT -> Dict.insert k (Toml.Table newT) container)

                Nothing ->
                    mergeDoc ( x, rest ) docToAdd Dict.empty
                        |> Result.map (\newT -> Dict.insert k (Toml.Table newT) container)

                Just (Toml.Array (Toml.ATable v)) ->
                    case Array.get (Array.length v - 1) v of
                        Just t ->
                            mergeDoc ( x, rest ) docToAdd t
                                |> Result.map (\newDoc -> Array.set (Array.length v - 1) newDoc v)
                                |> Result.map (\newArr -> Dict.insert k (Toml.Array (Toml.ATable newArr)) container)

                        Nothing ->
                            Err "empty array?"

                Just _ ->
                    Err "Adding to a non-table"


appendDoc : Key -> Toml.Document -> Toml.Document -> Result String Toml.Document
appendDoc ( k, rest ) docToAdd container =
    case rest of
        [] ->
            case Dict.get k container of
                Just (Toml.Array (Toml.ATable v)) ->
                    Array.push docToAdd v
                        |> Toml.ATable
                        |> Toml.Array
                        |> (\v -> Dict.insert k v container)
                        |> Ok

                Just _ ->
                    Err "Target is not an array"

                Nothing ->
                    Array.push docToAdd Array.empty
                        |> Toml.ATable
                        |> Toml.Array
                        |> (\v -> Dict.insert k v container)
                        |> Ok

        [ x ] ->
            case Dict.get k container of
                Just (Toml.Array (Toml.ATable v)) ->
                    case Array.get (Array.length v - 1) v of
                        Just t ->
                            appendDoc ( x, [] ) docToAdd t
                                |> Result.map (\newDoc -> Array.set (Array.length v - 1) newDoc v)
                                |> Result.map (\newTable -> Dict.insert k (Toml.Array (Toml.ATable newTable)) container)

                        Nothing ->
                            appendDoc ( x, [] ) docToAdd Dict.empty
                                |> Result.map (\newDoc -> Array.set (Array.length v - 1) newDoc v)
                                |> Result.map (\newTable -> Dict.insert k (Toml.Array (Toml.ATable newTable)) container)

                Just (Toml.Table t) ->
                    appendDoc ( x, [] ) docToAdd t
                        |> Result.map (\v -> Dict.insert k (Toml.Table v) container)

                Nothing ->
                    appendDoc ( x, [] ) docToAdd Dict.empty
                        |> Result.map (\v -> Dict.insert k (Toml.Table v) container)

                Just _ ->
                    Err "Target is not a table"

        x :: xs ->
            case Dict.get k container of
                Just (Toml.Table t) ->
                    appendDoc ( x, xs ) docToAdd t
                        |> Result.map (\v -> Dict.insert k (Toml.Table v) container)

                Nothing ->
                    appendDoc ( x, xs ) docToAdd Dict.empty
                        |> Result.map (\v -> Dict.insert k (Toml.Table v) container)

                Just _ ->
                    Err "Target is not a table"



-- Parsing entries


parseEntry : Parser Entry
parseEntry =
    oneOf
        [ map Empty comment
        , map Empty eol
        , map (Header True) arrayOfTablesHeader
        , map (Header False) tableHeader
        , map KVPair (lazy (\_ -> kvPair)) |. eolOrComment
        ]


comment : Parser String
comment =
    inContext "comment" <|
        succeed identity
            |. symbol "#"
            |= keep zeroOrMore ((/=) '\n')
            |. eol


eol : Parser String
eol =
    oneOf [ keep oneOrMore ((==) '\n'), succeed "end" |. end ]


arrayOfTablesHeader : Parser Key
arrayOfTablesHeader =
    header "[[" "]]"


tableHeader : Parser Key
tableHeader =
    header "[" "]"


header : String -> String -> Parser Key
header open close =
    succeed identity
        |. symbol open
        |. ws
        |= key
        |. ws
        |. symbol close
        |. ws
        |. eolOrComment


kvPair : Parser ( Key, Toml.Value )
kvPair =
    inContext "key value pair" <|
        succeed (,)
            |= key
            |. symbol "="
            |. ws
            |= lazy (\_ -> value)


eolOrComment : Parser String
eolOrComment =
    oneOf
        [ eol
        , comment
        ]



-- Parsing keys


key : Parser Key
key =
    let
        rest : String -> List String -> Parser ( String, List String )
        rest k xs =
            oneOf
                [ succeed identity
                    |. ws
                    |= keyPart
                    |> delayedCommit (symbol ".")
                    |> andThen (\x -> rest k (x :: xs))
                , succeed ( k, List.reverse xs )
                ]
    in
    keyPart |> andThen (\k -> rest k [])


keyPart : Parser String
keyPart =
    oneOf
        [ bareKey
        , singleQuotedKey
        , doublyQuotedKey
        ]
        |. ws


bareKey : Parser String
bareKey =
    succeed identity
        |= keep oneOrMore isKeyChar


isKeyChar : Char -> Bool
isKeyChar c =
    Char.isUpper c
        || Char.isLower c
        || Char.isDigit c
        || List.member c [ '_', '-' ]


singleQuotedKey : Parser String
singleQuotedKey =
    literalString


doublyQuotedKey : Parser String
doublyQuotedKey =
    basicString



-- Parsing values


value : Parser Toml.Value
value =
    inContext "value" <|
        oneOf
            [ map Toml.String string
            , map Toml.Bool bool
            , map Toml.DateTime dateTime
            , map Toml.LocalDateTime localDateTime
            , map Toml.LocalDate localDate
            , map Toml.LocalTime localTime
            , map Toml.Float float
            , map Toml.Int int
            , map Toml.Array array
            , map Toml.Table (lazy (\_ -> table))
            ]
            |. ws



-- parsing a local date


localDate : Parser Calendar.Date
localDate =
    inContext "local date" <|
        succeed Calendar.Date
            |= delayedCommitMap (\year _ -> year) (dtDigits (Exactly 4)) (symbol "-")
            |= dtDigits (Exactly 2)
            |. symbol "-"
            |= dtDigits (Exactly 2)


digits : Count -> Parser String
digits count =
    keep count Char.isDigit


dtDigits : Count -> Parser Int
dtDigits count =
    digits count
        |> andThen (String.toInt >> result fail succeed)



-- parsing a local time


localTime : Parser Calendar.Time
localTime =
    inContext "local time" <|
        succeed Calendar.Time
            |= delayedCommitMap (\hours _ -> hours) (dtDigits (Exactly 2)) (symbol ":")
            |= dtDigits (Exactly 2)
            |. symbol ":"
            |= seconds


seconds : Parser Float
seconds =
    dtDigits (Exactly 2)
        |> andThen maybeFractionalSeconds


maybeFractionalSeconds : Int -> Parser Float
maybeFractionalSeconds integerPart =
    oneOf
        [ succeed makeFloat
            |. symbol "."
            |= succeed integerPart
            |= digits oneOrMore
        , succeed (toFloat integerPart)
        ]



-- parsing a local datetime


localDateTime : Parser Calendar.LocalDateTime
localDateTime =
    inContext "local datetime" <|
        succeed Calendar.LocalDateTime
            |= delayedCommitMap (\date _ -> date)
                localDate
                (oneOf [ symbol "t", symbol "T", symbol " " ])
            |= localTime



-- parsing an instant / datetime with timezone


dateTime : Parser Calendar.DateTime
dateTime =
    inContext "datetime" <|
        delayedCommitMap
            (\{ date, time } offset ->
                { date = date, time = time, offset = offset }
            )
            localDateTime
            offset


offset : Parser Calendar.Offset
offset =
    oneOf
        [ symbol "Z" |> map (always emptyOffset)
        , symbol "z" |> map (always emptyOffset)
        , numOffset
        ]


emptyOffset : Calendar.Offset
emptyOffset =
    { hours = 0, minutes = 0 }


numOffset : Parser Calendar.Offset
numOffset =
    succeed (\s h m -> { hours = applySign s h, minutes = m })
        |= sign
        |= dtDigits (Exactly 2)
        |. symbol ":"
        |= dtDigits (Exactly 2)



-- parsing bools


bool : Parser Bool
bool =
    inContext "bool" <|
        oneOf
            [ map (always True) (keyword "true")
            , map (always False) (keyword "false")
            ]



-- parsing integers


int : Parser Int
int =
    inContext "int" <|
        oneOf
            [ hexInt
            , octalInt
            , binaryInt
            , literalInt
            ]
            |. ws


literalInt : Parser Int
literalInt =
    succeed applySign
        |= optionalSign
        |= oneOf
            [ symbol "0" |> map (always 0)
            , strictlyPositiveInt
            ]


strictlyPositiveInt : Parser Int
strictlyPositiveInt =
    let
        rest : String -> Parser Int
        rest acc =
            oneOf
                [ succeed identity
                    |. optional (symbol "_")
                    |= keep oneOrMore Char.isDigit
                    |> andThen (\c -> rest (acc ++ c))
                , String.toInt acc
                    |> result fail succeed
                ]
    in
    keep oneOrMore (\x -> Char.isDigit x && x /= '0')
        |> andThen rest


hexInt : Parser Int
hexInt =
    nBaseInt "0x" hexDigit 16


octalInt : Parser Int
octalInt =
    nBaseInt "0o" octalDigit 8


binaryInt : Parser Int
binaryInt =
    nBaseInt "0b" bit 2


nBaseInt : String -> Parser Int -> Int -> Parser Int
nBaseInt prefix digit multiplier =
    let
        rest : Int -> Parser Int
        rest acc =
            oneOf
                [ succeed identity
                    |. optional (symbol "_")
                    |= digit
                    |> andThen (\b -> acc * multiplier + b |> rest)
                , succeed acc
                ]
    in
    succeed identity
        |. symbol prefix
        |= digit
        |> andThen rest


bit : Parser Int
bit =
    oneOf
        [ symbol "1" |> map (always 1)
        , symbol "0" |> map (always 0)
        ]


octalDigit : Parser Int
octalDigit =
    oneOf
        [ bit
        , symbol "2" |> map (always 2)
        , symbol "3" |> map (always 3)
        , symbol "4" |> map (always 4)
        , symbol "5" |> map (always 5)
        , symbol "6" |> map (always 6)
        , symbol "7" |> map (always 7)
        ]


hexDigit : Parser Int
hexDigit =
    oneOf
        [ octalDigit
        , symbol "8" |> map (always 8)
        , symbol "9" |> map (always 9)
        , symbol "a" |> map (always 10)
        , symbol "A" |> map (always 10)
        , symbol "b" |> map (always 11)
        , symbol "B" |> map (always 11)
        , symbol "c" |> map (always 12)
        , symbol "C" |> map (always 12)
        , symbol "d" |> map (always 13)
        , symbol "D" |> map (always 13)
        , symbol "e" |> map (always 14)
        , symbol "E" |> map (always 14)
        , symbol "f" |> map (always 15)
        , symbol "F" |> map (always 15)
        ]



--parsing floats


float : Parser Float
float =
    inContext "float" <|
        oneOf
            [ fractional
            , exponential
            , infinity
            , nan
            ]
            |. ws


fractional : Parser Float
fractional =
    succeed identity
        |. symbol "."
        |= fractionalPart
        |> delayedCommitMap finishFloat integerPart
        |> andThen maybeExponentiate


exponential : Parser Float
exponential =
    delayedCommitMap applyExponent
        signedFloatPart
        (succeed identity
            |. oneOf [ symbol "e", symbol "E" ]
            |= signedFloatPart
        )


signedFloatPart : Parser Float
signedFloatPart =
    map (\( s, v ) -> toFloat (applySign s v))
        integerPart


maybeExponentiate : Float -> Parser Float
maybeExponentiate n =
    oneOf
        [ exponent |> map (applyExponent n)
        , succeed n
        ]


applyExponent : Float -> Float -> Float
applyExponent coeff exp =
    coeff * (10 ^ exp)


makeFloat : Int -> String -> Float
makeFloat integerPart fractionalPart =
    toFloat integerPart + toFractional fractionalPart


finishFloat : ( Sign, Int ) -> String -> Float
finishFloat ( sign, integerPart ) fractionalPart =
    applySign sign (makeFloat integerPart fractionalPart)


toFractional : String -> Float
toFractional floatString =
    floatString
        |> String.toInt
        |> Result.withDefault 0
        |> dividedBy (10 ^ String.length floatString)


exponent : Parser Float
exponent =
    succeed identity
        |. oneOf [ symbol "e", symbol "E" ]
        |= signedFloatPart


dividedBy : Int -> Int -> Float
dividedBy divisor dividend =
    toFloat dividend / toFloat divisor


integerPart : Parser ( Sign, Int )
integerPart =
    succeed (,)
        |= optionalSign
        |= oneOf
            [ symbol "0" |> map (always 0)
            , strictlyPositiveInt
            ]


fractionalPart : Parser String
fractionalPart =
    let
        rest : String -> Parser String
        rest acc =
            oneOf
                [ succeed identity
                    |. optional (symbol "_")
                    |= keep oneOrMore Char.isDigit
                    |> andThen (\s -> rest <| acc ++ s)
                , succeed acc
                ]
    in
    keep oneOrMore Char.isDigit
        |> andThen rest


infinity : Parser Float
infinity =
    delayedCommitMap (\s _ -> infinityVal s)
        optionalSign
        (symbol "inf")


infinityVal : Sign -> Float
infinityVal sign =
    case sign of
        Pos ->
            1 / 0

        Neg ->
            -1 / 0


nan : Parser Float
nan =
    delayedCommitMap (\_ _ -> nanVal)
        optionalSign
        (symbol "nan")


nanVal : Float
nanVal =
    0 / 0



-- number helpers


type Sign
    = Pos
    | Neg


sign : Parser Sign
sign =
    oneOf
        [ symbol "+" |> map (always Pos)
        , symbol "-" |> map (always Neg)
        ]


optionalSign : Parser Sign
optionalSign =
    oneOf
        [ sign
        , succeed Pos
        ]


applySign : Sign -> number -> number
applySign sign val =
    case sign of
        Pos ->
            val

        Neg ->
            negate val



-- parsing array values


array : Parser Toml.ArrayValue
array =
    let
        self : Parser Toml.ArrayValue
        self =
            lazy (\_ -> array)
    in
    inContext "array" <|
        succeed identity
            |. symbol "["
            |. arrWs
            |= oneOf
                [ start Toml.AString string
                , start Toml.ABool bool
                , start Toml.ADateTime dateTime
                , start Toml.ALocalDateTime localDateTime
                , start Toml.ALocalDate localDate
                , start Toml.ALocalTime localTime
                , start Toml.AFloat float
                , start Toml.AInt int
                , start Toml.ATable (lazy (\_ -> table))
                , start Toml.AArray self
                , succeed Toml.AEmpty
                ]
            |. arrWs
            |. symbol "]"
            |. ws


start :
    (Array a -> Toml.ArrayValue)
    -> Parser a
    -> Parser Toml.ArrayValue
start toArrVal elem =
    elem
        |> andThen
            (\first ->
                rest toArrVal elem (Array.push first Array.empty)
            )


rest :
    (Array a -> Toml.ArrayValue)
    -> Parser a
    -> Array a
    -> Parser Toml.ArrayValue
rest toArrVal elem acc =
    oneOf
        [ delayedCommit arraySep elem
            |> andThen (\x -> rest toArrVal elem (Array.push x acc))
        , succeed (toArrVal acc)
            |. arrWs
            |. optional (symbol ",")
        ]


arraySep : Parser ()
arraySep =
    succeed ()
        |. arrWs
        |. symbol ","
        |. arrWs


arrWs : Parser ()
arrWs =
    repeat oneOrMore (oneOf [ map (always ()) eolOrComment, reqWs ])
        |> optional



--parsing inline tables


table : Parser Toml.Document
table =
    inContext "inline table" <|
        succeed identity
            |. symbol "{"
            |. ws
            |= lazy (\_ -> tablePairs)
            |. symbol "}"
            |. ws


tablePairs : Parser Toml.Document
tablePairs =
    oneOf
        [ lazy (\_ -> kvPair)
            |. ws
            |> andThen
                (\( k, v ) ->
                    case addKVPair k v Dict.empty of
                        Err e ->
                            fail e

                        Ok doc ->
                            nextPairs doc
                )
        , succeed Dict.empty
        ]


nextPairs : Toml.Document -> Parser Toml.Document
nextPairs doc =
    oneOf
        [ succeed identity
            |. symbol ","
            |. ws
            |= lazy (\_ -> kvPair)
            |. ws
            |> andThen
                (\( k, v ) ->
                    case addKVPair k v doc of
                        Err e ->
                            fail e

                        Ok newDoc ->
                            nextPairs newDoc
                )
        , succeed doc
        ]



--parsing strings


string : Parser String
string =
    inContext "string" <|
        oneOf
            [ multilineLiteralString
            , multilineBasicString
            , literalString
            , basicString
            ]


multilineLiteralString : Parser String
multilineLiteralString =
    inContext "multiline literal string" <|
        succeed identity
            |. symbol "'''"
            |. optional (symbol "\n")
            |= until "'''"


until : String -> Parser String
until marker =
    ignoreUntil marker
        |> source
        |> map (String.slice 0 (negate (String.length marker)))


literalString : Parser String
literalString =
    succeed identity
        |. symbol "'"
        |= keep zeroOrMore (\c -> c /= '\'' && c /= '\n')
        |. symbol "'"


multilineBasicString : Parser String
multilineBasicString =
    succeed identity
        |. symbol "\"\"\""
        |. optional (symbol "\n")
        |= multilineBasicStringContent ""


multilineBasicStringContent : String -> Parser String
multilineBasicStringContent acc =
    let
        {- This little trick allows us to piece the parts together. -}
        continue : String -> Parser String
        continue string =
            multilineBasicStringContent <| acc ++ string
    in
    oneOf
        [ lineEndingBackslash |> andThen (\_ -> continue "")
        , succeed acc |. symbol "\"\"\""
        , {- This means that characters like `\n` must be escaped as `\\n`
             So a literal backslash followed by a literal `n`.
          -}
          escapedControlCharacter |> andThen continue

        {- Arbitrary unicode can be embedded using `\uXXXX` where the `X`s form
           a valid hexadecimal sequence.
        -}
        , escapedUnicode |> andThen continue

        {- Finally, we have the rest of unicode, specifically disallowing certain
           things: control characters, literal `\` and literal `"`.
        -}
        , nonControlCharacters |> andThen continue

        {- If none of the above produce anything, we succeed with what we've
           accumulated so far.
        -}
        , succeed acc
        ]


lineEndingBackslash : Parser ()
lineEndingBackslash =
    delayedCommit (symbol "\\" |. ws) eol
        |. repeat zeroOrMore (oneOf [ map (always ()) eol, reqWs ])
        |> map (always ())


basicString : Parser String
basicString =
    succeed identity
        |. symbol "\""
        |= regularStringContent ""
        |. symbol "\""


regularStringContent : String -> Parser String
regularStringContent acc =
    let
        {- This little trick allows us to piece the parts together. -}
        continue : String -> Parser String
        continue string =
            regularStringContent <| acc ++ string
    in
    oneOf
        [ {- First things first, escaped control characters.
             This means that characters like `\n` must be escaped as `\\n`
             So a literal backslash followed by a literal `n`.
          -}
          escapedControlCharacter |> andThen continue

        {- Arbitrary unicode can be embedded using `\uXXXX` where the `X`s form
           a valid hexadecimal sequence.
        -}
        , escapedUnicode |> andThen continue

        {- Finally, we have the rest of unicode, specifically disallowing certain
           things: control characters, literal `\` and literal `"`.
        -}
        , nonControlCharacters |> andThen continue

        {- If none of the above produce anything, we succeed with what we've
           accumulated so far.
        -}
        , succeed acc
        ]


escapedControlCharacter : Parser String
escapedControlCharacter =
    -- TODO: check TOML spec if this is correct
    [ ( "\\\"", "\"" )
    , ( "\\\\", "\\" )
    , ( "\\b", "\x08" )
    , ( "\\f", "\x0C" )
    , ( "\\n", "\n" )
    , ( "\\r", "\x0D" )
    , ( "\\t", "\t" )
    ]
        |> List.map symbolicString
        |> oneOf


symbolicString : ( String, String ) -> Parser String
symbolicString ( expected, replacement ) =
    succeed replacement |. symbol expected


escapedUnicode : Parser String
escapedUnicode =
    succeed (\a b -> foldHex a 0 |> foldHex b |> String.UTF32.byteToString)
        |. oneOf [ symbol "\\u", symbol "\\U" ]
        |= hexQuad
        |= oneOf [ hexQuad, succeed [] ]


foldHex : List Int -> Int -> Int
foldHex xs n =
    List.foldl (\x acc -> acc * 16 + x) n xs


hexQuad : Parser (List Int)
hexQuad =
    repeat (Exactly 4) hexDigit


nonControlCharacters : Parser String
nonControlCharacters =
    {- So basically, anything other than control characters, literal backslashes
       (which are already handled, unless it's invalid toml), and the closing `"`
    -}
    keep oneOrMore
        (noneMatch [ (==) '"', (==) '\\', Char.toCode >> isControlChar ])


isControlChar : Char.KeyCode -> Bool
isControlChar keyCode =
    (keyCode < 0x20) || (keyCode == 0x7F)



-- generic helpers


ws : Parser ()
ws =
    ignore zeroOrMore (chars [ ' ', '\t' ])


reqWs : Parser ()
reqWs =
    ignore oneOrMore (chars [ ' ', '\t' ])


optional : Parser a -> Parser ()
optional parser =
    oneOf
        [ map (always ()) parser
        , succeed ()
        ]


result : (e -> v) -> (a -> v) -> Result e a -> v
result onErr onOk res =
    case res of
        Ok v ->
            onOk v

        Err e ->
            onErr e


chars : List Char -> Char -> Bool
chars xs x =
    List.member x xs


noneMatch : List (Char -> Bool) -> Char -> Bool
noneMatch matchers c =
    not <| List.any (\m -> m c) matchers
