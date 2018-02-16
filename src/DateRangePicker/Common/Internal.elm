module DateRangePicker.Common.Internal
    exposing
        ( FullYear
        , Quarter
        , EnabledDateRange
        , (?>)
        , ($!)
        , chunksOfLeft
        )

{-| A common internal library between DatePicker and DateRangePicker

@docs FullYear, Quarter

-}

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)


{-| An opaque type to represent the full year that the daterangepicker is using.
-}
type alias FullYear =
    { name : String
    , year : Int
    , quarters : List Quarter
    }


{-| An opaque type representing a quarter within the FullYear. Ex. (Jan, Feb, March) represents Q1.
-}
type alias Quarter =
    { name : String
    , months : List (List Date)
    }


{-| An opaque type representing the enabled dates for the datepicker
-}
type alias EnabledDateRange =
    { start : Maybe Date
    , end : Maybe Date
    }


{-| An opaque recursive function that chunks a list of a into a
list of lists of a of equal chunks.


## Example:

    chunksOfLeft 3 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] =
        [ [ 1, 2, 3 ]
        , [ 4.5, 6 ]
        , [ 7, 8, 9 ]
        , [ 10, 11, 12 ]
        ]

-}
chunksOfLeft : Int -> List a -> List (List a)
chunksOfLeft k xs =
    let
        len =
            List.length xs
    in
        if len > k then
            List.take k xs :: chunksOfLeft k (List.drop k xs)
        else
            [ xs ]


(?>) : Maybe a -> a -> a
(?>) =
    flip Maybe.withDefault


($!) : a -> List (Cmd b) -> ( a, Cmd b )
($!) model cmds =
    ( model, Cmd.batch cmds )
