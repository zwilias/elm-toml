module Toml.Calendar exposing (Date, DateTime, LocalDateTime, Offset, Time)


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


type alias Time =
    { hours : Int
    , minutes : Int
    , seconds : Float
    }


type alias LocalDateTime =
    { date : Date
    , time : Time
    }


type alias Offset =
    { hours : Int
    , minutes : Int
    }


type alias DateTime =
    { date : Date
    , time : Time
    , offset : Offset
    }
