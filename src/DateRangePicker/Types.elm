module DateRangePicker.Types exposing (CalendarDisplay(..), CalendarRange, DateRange, EnabledDateRange, Months, Quarter, RestrictedDateRange(..), Year)

{-| An opaque type to represent the displayed calendar range that the daterange picker is using.
-}

import Date exposing (Date, Month)


type alias Year =
    Int


{-| A type representing a restricted range for the datepicker. All dates not within the restricted date range will be disabled.

  - _Off_ = no restrictions, any date to any date can be chosen.
  - _ToPresent_ = from any date in the past up to today (including today)
  - _FromPresent_ = from today to any date in the future
  - _Past_ = from any date in the past up to yesterday (excluding today)
  - _Future_ = from tomorrow up to any date in the future
  - _Between_ date date = only between the two given dates [start - end] (inclusive)
  - _To_ date = from any date in the past up to the given date (inclusive)
  - _From_ date = from the given date up to any date in the future (inclusive)

-}
type RestrictedDateRange
    = Off
    | ToPresent
    | FromPresent
    | Past
    | Future
    | Between Date Date
    | To Date
    | From Date


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


{-| A type representing how the calendar will be displayed

  - _FullCalendar_ = the datepicker displays all 12 months when open
  - _ThreeMonths_ = the datepicker displays 3 months at a time
  - _TwoMonths_ = the datepicker displays 2 months at a time
  - _OneMonth_ = the datepicker displays 1 month at a time

-}
type CalendarDisplay
    = FullCalendar
    | ThreeMonths
    | TwoMonths
    | OneMonth
