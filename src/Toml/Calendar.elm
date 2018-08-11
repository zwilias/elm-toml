module Toml.Calendar exposing (Date, DateTime, LocalDateTime, Offset, Time)

{-| Basic data-structures to model dates, times, datetimes and datetimes with an
offset.

The plan is to (eventually) turn these into opaque structures and validate their
format before being able to construct them. For now, they hold no guarantees of
correctness.

@docs Date, Time, LocalDateTime, Offset, DateTime

-}


{-| Represent a date.
-}
type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


{-| Represent a time.
-}
type alias Time =
    { hours : Int
    , minutes : Int
    , seconds : Float
    }


{-| Represent a floating datetime, with unknown offset.
-}
type alias LocalDateTime =
    { date : Date
    , time : Time
    }


{-| Represent an UTC offset.
-}
type alias Offset =
    { hours : Int
    , minutes : Int
    }


{-| Represent a datetime, with a known UTC offset.
-}
type alias DateTime =
    { date : Date
    , time : Time
    , offset : Offset
    }
