module Toml exposing (ArrayValue(..), Document, Value(..))

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)


type alias Document =
    Dict String Value


type Value
    = String String
    | Int Int
    | Float Float
    | Bool Bool
    | Array ArrayValue
    | Table Document


type ArrayValue
    = AString (Array String)
    | ABool (Array Bool)
    | AInt (Array Int)
    | AFloat (Array Float)
    | ATable (Array Document)
    | AArray (Array ArrayValue)
    | AEmpty
