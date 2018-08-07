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
        , inContext
        , keep
        , keyword
        , lazy
        , map
        , oneOf
        , oneOrMore
        , repeat
        , succeed
        , symbol
        , zeroOrMore
        )
import Toml


{-| Run the parser and extract the final result
-}
parse : String -> Result Parser.Error Toml.Document
parse input =
    input
        |> Parser.run (document emptyAcc)
        |> Result.map .final



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
    = Empty
    | KVPair ( Key, Toml.Value )
    | Header Bool Key


emptyAcc : Acc
emptyAcc =
    { final = Dict.empty
    , current = Dict.empty
    , context = []
    , isArray = False
    }



-- Parsing the document


document : Acc -> Parser Acc
document acc =
    succeed identity
        |. ws
        |= parseEntry
        |> andThen (commit acc)
        |> andThen
            (\newAcc ->
                oneOf
                    [ end |> andThen (\_ -> finalize newAcc)
                    , lazy (\_ -> document newAcc)
                    ]
            )


commit : Acc -> Entry -> Parser Acc
commit acc entry =
    case entry of
        Empty ->
            succeed acc

        KVPair ( key, val ) ->
            case addKVPair key val acc.current of
                Ok newDoc ->
                    succeed { acc | current = newDoc }

                Err e ->
                    fail e

        Header isArray k ->
            -- TODO: finalize previous stuff
            succeed acc


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


finalize : Acc -> Parser Acc
finalize acc =
    case acc.context of
        [] ->
            succeed
                { acc
                    | final = Dict.union acc.final acc.current
                    , current = Dict.empty
                    , context = []
                    , isArray = False
                }

        k :: rest ->
            if acc.isArray then
                fail "no support for array tables yet"
            else
                case addKVPair ( k, rest ) (Toml.Table acc.current) acc.final of
                    Ok v ->
                        succeed
                            { acc
                                | final = v
                                , current = Dict.empty
                                , context = []
                                , isArray = False
                            }

                    Err e ->
                        fail e



-- Parsing entries


parseEntry : Parser Entry
parseEntry =
    oneOf
        [ map (\_ -> Empty) comment
        , map (\_ -> Empty) eol
        , map KVPair kvPair
        ]


comment : Parser String
comment =
    inContext "comment" <|
        succeed identity
            |. symbol "#"
            |= keep zeroOrMore ((/=) '\n')
            |. eol


eol : Parser ()
eol =
    oneOf [ ignore oneOrMore ((==) '\n'), end ]


kvPair : Parser ( Key, Toml.Value )
kvPair =
    inContext "key value pair" <|
        succeed (,)
            |= key
            |. symbol "="
            |. ws
            |= value
            |. eolOrComment


eolOrComment : Parser ()
eolOrComment =
    oneOf
        [ eol
        , map (\_ -> ()) comment
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
    regularString



-- Parsing values


value : Parser Toml.Value
value =
    inContext "value" <|
        oneOf
            [ map Toml.String string
            , map Toml.Bool bool
            , map Toml.Float float
            , map Toml.Int int
            , map Toml.Array array
            , map Toml.Table table
            ]
            |. ws



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
        |= sign
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
        |> delayedCommitMap makeFloat integerPart
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


makeFloat : ( Sign, Int ) -> String -> Float
makeFloat ( sign, integerPart ) fractionalPart =
    applySign sign (toFloat integerPart + toFractional fractionalPart)


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
        |= sign
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
        sign
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
        sign
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
                [ andThen (start Toml.AString string) string
                , andThen (start Toml.ABool bool) bool
                , andThen (start Toml.AFloat float) float
                , andThen (start Toml.AInt int) int
                , andThen (start Toml.AArray self) self
                , succeed Toml.AEmpty
                ]
            |. arrWs
            |. symbol "]"
            |. ws


start :
    (Array a -> Toml.ArrayValue)
    -> Parser a
    -> a
    -> Parser Toml.ArrayValue
start toArrVal elem first =
    rest toArrVal elem (Array.push first Array.empty)


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
    repeat oneOrMore (oneOf [ eolOrComment, reqWs ])
        |> optional



--parsing inline tables


table : Parser Toml.Document
table =
    inContext "inline table" <|
        fail "TODO"



--parsing strings


string : Parser String
string =
    inContext "string" <|
        oneOf [ literalString, regularString ]


literalString : Parser String
literalString =
    succeed identity
        |. symbol "'"
        |= keep zeroOrMore (\c -> c /= '\'' && c /= '\n')
        |. symbol "'"


regularString : Parser String
regularString =
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
    {- TOML (and soon, Elm) allow arbitrary UTF-16 codepoints to be written
       using `\uBEEF` syntax. These may also appear in escaped version in TOML,
       so a literal `\u` followed by 4 hexadecimal characters.

       This means something like a space may also be written as `\\u0020`

       TODO: TOML doesn't actually allow UTF-16 codepoints, but rather unicode
       scalar values
    -}
    succeed (Char.fromCode >> String.fromChar)
        |. symbol "\\u"
        |= hexQuad


hexQuad : Parser Int
hexQuad =
    keep (Exactly 4) Char.isHexDigit |> andThen hexQuadToInt


hexQuadToInt : String -> Parser Int
hexQuadToInt quad =
    ("0x" ++ quad)
        -- Kind of cheating here.
        |> String.toInt
        |> result fail succeed


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
