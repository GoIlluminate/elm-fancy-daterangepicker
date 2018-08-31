module DateRangePicker.Common
    exposing
        ( RestrictedDateRange(..)
        , DateRange
        , mkDateRange
        , inRange
        )

{-| A common library between DatePicker and DateRangePicker

@docs DateRange, mkDateRange, inRange


# Settings

@docs RestrictedDateRange

-}

import Date exposing (Date, Day(..), Month(..), day, month, year)
import DateRangePicker.Date
    exposing
        ( mkDate
        , dateGreaterThanOrEqualTo
        , dateLessThanOrEqualTo
        )


{-| A type representing a restricted range for the datepicker. All dates not within the restricted date range will be disabled.

  - *Off* = no restrictions, any date to any date can be chosen.
  - *ToPresent* = from any date in the past up to today (including today)
  - *FromPresent* = from today to any date in the future
  - *Past* = from any date in the past up to yesterday (excluding today)
  - *Future* = from tomorrow up to any date in the future
  - *Between* date date = only between the two given dates [start - end] (inclusive)
  - *To* date = from any date in the past up to the given date (inclusive)
  - *From* date = from the given date up to any date in the future (inclusive)

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


{-| A function that creates a DateRange by taking in two dates (start and end).

This function assumes that start <= end

-}
mkDateRange : Date -> Date -> DateRange
mkDateRange start end =
    { start = mkDate (year start) (month start) (day start)
    , end = mkDate (year end) (month end) (day end)
    }


{-| A function to check if a given date is within a
given dateRange.
-}
inRange : Date -> DateRange -> Bool
inRange date { start, end } =
    dateLessThanOrEqualTo start date && dateGreaterThanOrEqualTo end date
