module DateRangePicker.Common exposing
    ( DateRange, mkDateRange, inRange
    , yearsInRange, monthsInRange, weeksInRange, daysInRange
    , CalendarDisplay(..), RestrictedDateRange(..), calendarDisplayToClassStr, calendarDisplayToDisplayStr
    )

{-| A common library between DatePicker and DateRangePicker

@docs DateRange, mkDateRange, inRange
@docs yearsInRange, monthsInRange, weeksInRange, daysInRange


# Settings

@docs CalendarDisplay, RestrictedDateRange, calendarDisplayToClassStr, calendarDisplayToDisplayStr

-}

import Date exposing (Date, Unit(..), day, diff, fromCalendarDate, month, year)
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
calendarDisplayToClassStr : CalendarDisplay -> String
calendarDisplayToClassStr calendarDisplay =
    case calendarDisplay of
        FullCalendar ->
            "full-calendar"

        ThreeMonths ->
            "three-months"

        TwoMonths ->
            "two-months"

        OneMonth ->
            "one-month"


{-| A function that gets the display string for the CalendarDisplay
-}
calendarDisplayToDisplayStr : CalendarDisplay -> String
calendarDisplayToDisplayStr calendarDisplay =
    case calendarDisplay of
        FullCalendar ->
            "FullCalendar"

        ThreeMonths ->
            "ThreeMonths"

        TwoMonths ->
            "TwoMonths"

        OneMonth ->
            "OneMonth"


{-| A function that returns the number of years as a whole number in the daterange
-}
yearsInRange : DateRange -> Int
yearsInRange dateRange =
    diff Years dateRange.start dateRange.end


{-| A function that returns the number of months as a whole number in the daterange
-}
monthsInRange : DateRange -> Int
monthsInRange dateRange =
    diff Months dateRange.start dateRange.end


{-| A function that returns the number of weeks as a whole number in the daterange
-}
weeksInRange : DateRange -> Int
weeksInRange dateRange =
    diff Weeks dateRange.start dateRange.end


{-| A function that returns the number of days as a whole number in the daterange
-}
daysInRange : DateRange -> Int
daysInRange dateRange =
    diff Days dateRange.start dateRange.end
