module DateRangePicker.Common exposing
    ( DateRange, mkDateRange, inRange
    , CalendarDisplay(..), RestrictedDateRange(..), calendarDisplayToStr
    )

{-| A common library between DatePicker and DateRangePicker

@docs DateRange, mkDateRange, inRange


# Settings

@docs CalendarDisplay, RestrictedDateRange, calendarDisplayToStr

-}

import Date exposing (Date, day, fromCalendarDate, month, year)
import DateRangePicker.Date
    exposing
        ( dateGreaterThanOrEqualTo
        , dateLessThanOrEqualTo
        )
import Time exposing (Month(..), Weekday(..))


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


{-| A function that creates a DateRange by taking in two dates (start and end).

This function assumes that start <= end

-}
mkDateRange : Date -> Date -> DateRange
mkDateRange start end =
    { start = fromCalendarDate (year start) (month start) (day start)
    , end = fromCalendarDate (year end) (month end) (day end)
    }


{-| A function to check if a given date is within a
given dateRange.
-}
inRange : Date -> DateRange -> Bool
inRange date { start, end } =
    dateLessThanOrEqualTo start date && dateGreaterThanOrEqualTo end date


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


{-| A function that gets the class string for the CalendarDisplay
-}
calendarDisplayToStr : CalendarDisplay -> String
calendarDisplayToStr calendarDisplay =
    case calendarDisplay of
        FullCalendar ->
            "full-calendar"

        ThreeMonths ->
            "three-months"

        TwoMonths ->
            "two-months"

        OneMonth ->
            "one-month"
