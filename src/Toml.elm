module Toml exposing (ArrayValue(..), Document, Value(..))

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Toml.Calendar as Calendar


type alias Document =
    Dict String Value


type Value
    = String String
    | Int Int
    | Float Float
    | Bool Bool
    | LocalDate Calendar.Date
    | LocalTime Calendar.Time
    | LocalDateTime Calendar.LocalDateTime
    | DateTime Calendar.DateTime
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
