module DateRangePicker.Types exposing (CalendarRange, DateRange, EnabledDateRange, Months, Quarter, Year)

{-| An opaque type to represent the displayed calendar range that the daterange picker is using.
-}

import Date exposing (Date, Month)


type alias Year =
    Int


{-| A type representing a date range with a start date and end date.
-}
type alias DateRange =
    { start : Date
    , end : Date
    }


type alias CalendarRange =
    { name : String
    , start : Date
    , end : Date
    , months : Months
    }


{-| An opaque type to represent a list of a list of dates (months).
-}
type alias Months =
    List (List Date)


{-| An opaque type representing a quarter within the FullYear. Ex. (Jan, Feb, March) represents Q1.
-}
type alias Quarter =
    ( Month, Month, Month )


{-| An opaque type representing the enabled dates for the datepicker
-}
type alias EnabledDateRange =
    { start : Maybe Date
    , end : Maybe Date
    }
