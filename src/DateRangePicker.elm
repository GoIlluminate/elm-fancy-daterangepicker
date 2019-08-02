module DateRangePicker exposing
    ( Msg, DateRangePicker
    , init, update, subscriptions, isOpen, setOpen, view, getDateRange, setDateRange, getToday, CalendarDisplay(..), DateRange, RestrictedDateRange(..), daysInRange, inRange, mkDateRange, mkEnabledDateRangeFromRestrictedDateRange, monthAbbr, monthsInRange, setDisableRange, startOfQuarter, weeksInRange, yearsInRange
    , Settings, defaultSettings, setSettings, setDateRangeFormat, setPlaceholder, setInputName, setInputText, setInputId, setInputIcon, setInputAttributes, setPresetOptions, setRestrictedDateRange, formatDateRange, getMinDate, getMaxDate, setCalendarDisplay, setInputView
    , PresetOptions, PresetOption(..), Preset, PresetSetting, PresetInterval(..), PresetRelativeToToday(..), defaultPresetOptions, defaultPresets, mkPresetFromDateRange, mkPresetFromDates, getPresets, PresetType(..), defaultSingleSelectPresets, getSelectedPreset
    )

{-| A customizable daterangepicker component.

@docs Msg, DateRangePicker
@docs init, update, subscriptions, isOpen, setOpen, view, getDateRange, setDateRange, getToday, CalendarDisplay, DateRange, RestrictedDateRange, daysInRange, inRange, mkDateRange, mkEnabledDateRangeFromRestrictedDateRange, monthAbbr, monthsInRange, setDisableRange, startOfQuarter, weeksInRange, yearsInRange


# Settings

@docs Settings, defaultSettings, setSettings, setDateRangeFormat, setPlaceholder, setInputName, setInputText, setInputId, setInputIcon, setInputAttributes, setPresetOptions, setRestrictedDateRange, formatDateRange, getMinDate, getMaxDate, setCalendarDisplay, setInputView


## Presets

@docs PresetOptions, PresetOption, Preset, PresetSetting, PresetInterval, PresetRelativeToToday, defaultPresetOptions, defaultPresets, mkPresetFromDateRange, mkPresetFromDates, getPresets, PresetType, defaultSingleSelectPresets, getSelectedPreset

-}

import Browser.Events
import Date exposing (Date, day, fromCalendarDate, month, year)
import DateRangePicker.Helper
    exposing
        ( chunksOfLeft
        , dateEqualTo
        , dateGreaterThanOrEqualTo
        , dateLessThanOrEqualTo
        , dayToInt
        , endOfMonth
        , formatDate
        , formatMonth
        , inRange
        , initDate
        , isDisabledDate
        , noPresets
        , onClickNoDefault
        , padMonthLeft
        , padMonthRight
        , renderDaysOfWeek
        , startOfMonth
        )
import DateRangePicker.Types exposing (CalendarRange, EnabledDateRange, Months)
import Html exposing (Html, div, i, span, table, tbody, td, text, thead, tr)
import Html.Attributes as Attrs
import Html.Events
import Json.Decode as Json
import List.Extra as LE
import Task
import Time exposing (Month(..), Weekday(..))


{-| A type representing a date range with a start date and end date.
-}
type alias DateRange =
    DateRangePicker.Types.DateRange


{-| A type representing msgs that are fired from the update function.
-}
type Msg
    = InitCurrentDate Date
    | PrevCalendarRange
    | NextCalendarRange
    | StartSelection Date
    | EndSelection (Maybe Date)
    | SelectSingleDate Date
    | SelectDateRange DateRange
    | StartSelectionOnShift String
    | CancelShift String
    | UpdateCounter
    | TerminateBadState
    | SelectPresetOption Preset
    | Done
    | DoNothing
    | ToggleDateRangePicker
    | Clear
    | HoverDay Date


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


{-| The opaque model to be used within the DateRangePicker.
-}
type alias Model =
    { today : Date
    , inputText : Maybe String
    , open : Bool
    , dateRange : Maybe DateRange
    , startDate : Maybe Date
    , endDate : Maybe Date
    , hoveredDate : Maybe Date
    , showPresets : Bool
    , presets : List Preset
    , enabledDateRange : Maybe EnabledDateRange
    , settings : Settings
    , calendarRange : CalendarRange
    , isMouseDown : Bool
    , isShiftDown : Bool
    , selectedPreset : PresetType
    , terminationCounter : Int
    }


{-| The settings that the DateRangePicker uses.
-}
type alias Settings =
    { placeholder : String
    , inputName : Maybe String
    , inputId : Maybe String
    , inputIcon : Maybe (Html Msg)
    , inputAttributes : List (Html.Attribute Msg)
    , presetOptions : PresetOptions
    , restrictedDateRange : RestrictedDateRange
    , formatDateRange : DateRange -> String
    , calendarDisplay : CalendarDisplay
    , inputView : Maybe (String -> List (Html Msg))
    , disableRange : Bool
    }


{-| A type representing your preset options for your date range picker.
-}
type alias PresetOptions =
    { presetOption : PresetOption
    , presetSettings : List PresetSetting
    , presets : List Preset
    }


{-| A type representing which presets to use.

  - _DefaultPresets_ = Use the default presets from defaultPresets only.
  - _CustomPresetsFromSettings_ = Use only your custom built presets from a list of PresetSetting only.
  - _CustomPresets_ = Use only your custom built Presets.
  - _CustomOnly_ = Use only your custom build presets build from PresetSetting and your custom presets.
  - _AllPresets_ = Use all presets (default, customFromSettings, custom).
  - _NoPresets_ = Turn off Presets.

-}
type PresetOption
    = DefaultPresets
    | CustomPresetsFromSettings
    | CustomPresets
    | CustomOnly
    | AllPresets
    | NoPresets


{-| A type representing what the value in PresetSettings is measured in.

  - Ex. value = 1 and PresetInterval = Days, this is interpretted as 1 Days.
  - Ex. value = 4 and PresetI:nterval = Months, this is interpretted as 4 Months.

-}
type PresetInterval
    = Days
    | Months
    | Years


{-| A type representing how the preset is relative to today.

  - If using ToToday, the preset daterange would use today as the end date, and the date from your PresetSettings as the end date.
  - If using FromToday, the preset daterange would use today as the start date and the date from your PresetSettings as the end date.

-}
type PresetRelativeToToday
    = ToToday
    | FromToday


{-| A type used to generate preset dateranges.

  - _name_ = The name that you want to give the preset. i.e. "Past Month"
  - _interval_ = The interval in which you want to add/subtract the value from today.
  - _presetRelativeToToday_ = whether it is a range from [past - present] (ToToday) or [present - future] (FromToday)
  - _value_ = the number of your @interval that you are adding/subtracting.


## Example

    { name = "Past Month"
    , interval = Months
    , presetRelativeToday = ToToday
    , value = 1
    }

-}
type alias PresetSetting =
    { name : String
    , interval : PresetInterval
    , presetRelativeToToday : PresetRelativeToToday
    , value : Int
    }


{-| A type that can be used for calculating relative presets outside of the daterangepicker.
-}
type PresetType
    = Today
    | Yesterday
    | PastWeek
    | PastMonth
    | PastYear
    | WeeksFromToday Int
    | MonthsFromToday Int
    | EndOfTime
    | NoneSelected


{-| A type that represents a preset daterange.

  - _name_ = Name of the preset. i.e. "Past Month"
  - _dateRange_ = The daterange that is selected when selecting the preset.

-}
type alias Preset =
    { name : String
    , dateRange : DateRange
    , presetDateOption : PresetType
    }


{-| The DateRangePicker model.
-}
type DateRangePicker
    = DateRangePicker Model


{-| An opaque function to make presets from settings and a date
-}
mkPresets : Settings -> Date -> List Preset
mkPresets settings date =
    let
        presetOptions =
            settings.presetOptions

        defaultPresets_ =
            if settings.disableRange then
                defaultSingleSelectPresets date

            else
                defaultPresets date

        customPresetsFromSettings_ =
            List.map (mkPresetFromSetting date) presetOptions.presetSettings
    in
    case presetOptions.presetOption of
        DefaultPresets ->
            defaultPresets_

        CustomPresetsFromSettings ->
            customPresetsFromSettings_

        CustomPresets ->
            presetOptions.presets

        CustomOnly ->
            List.concat
                [ customPresetsFromSettings_
                , presetOptions.presets
                ]

        AllPresets ->
            List.concat
                [ defaultPresets_
                , customPresetsFromSettings_
                , presetOptions.presets
                ]

        NoPresets ->
            []


{-| An opaque function that creates a Preset from a PresetSetting
-}
mkPresetFromSetting : Date -> PresetSetting -> Preset
mkPresetFromSetting today { name, interval, presetRelativeToToday, value } =
    let
        start =
            case presetRelativeToToday of
                FromToday ->
                    today

                ToToday ->
                    case interval of
                        Days ->
                            Date.add Date.Days (value * -1) today

                        Months ->
                            Date.add Date.Months (value * -1) today

                        Years ->
                            Date.add Date.Years (value * -1) today

        end =
            case presetRelativeToToday of
                FromToday ->
                    case interval of
                        Days ->
                            Date.add Date.Days value today

                        Months ->
                            Date.add Date.Months value today

                        Years ->
                            Date.add Date.Years value today

                ToToday ->
                    today
    in
    { name = name
    , dateRange = mkDateRange start end
    , presetDateOption = NoneSelected
    }


{-| A function that creates a Preset from a name and a dateRange
-}
mkPresetFromDateRange : String -> DateRange -> PresetType -> Preset
mkPresetFromDateRange name dateRange preset =
    { name = name
    , dateRange = dateRange
    , presetDateOption = preset
    }


{-| A function that creates a Preset from a name, startDate, and endDate
-}
mkPresetFromDates : String -> Date -> Date -> PresetType -> Preset
mkPresetFromDates name start end preset =
    { name = name
    , dateRange = mkDateRange start end
    , presetDateOption = preset
    }


{-| An opaque function used to make the default presets
-}
defaultPresets : Date -> List Preset
defaultPresets today =
    [ presetToday today
    , presetYesterday today
    , presetPastWeek today
    , presetPastMonth today
    , presetPastYear today
    ]


{-| An opaque function used to make the default presets
-}
defaultSingleSelectPresets : Date -> List Preset
defaultSingleSelectPresets today =
    [ presetOneWeekFromToday today
    , presetTwoWeeksFromToday today
    , presetOneMonthFromToday today
    , presetThreeMonthsFromToday today
    , presetSixMonthsFromToday today
    , presetEndOfTime
    ]


{-| An opaque function for the default preset "Today"
-}
presetToday : Date -> Preset
presetToday today =
    mkPresetFromDates "Today" today today Today


{-| An opaque function for the default preset "Yesterday"
-}
presetYesterday : Date -> Preset
presetYesterday today =
    let
        start =
            Date.add Date.Days -1 today

        end =
            Date.add Date.Days -1 today
    in
    mkPresetFromDates "Yesterday" start end Yesterday


{-| An opaque function for the default preset "Past Week"
-}
presetPastWeek : Date -> Preset
presetPastWeek today =
    let
        start =
            Date.add Date.Days -7 today

        end =
            today
    in
    mkPresetFromDates "Past Week" start end PastWeek


{-| An opaque function for the default preset "Past Month"
-}
presetPastMonth : Date -> Preset
presetPastMonth today =
    let
        start =
            Date.add Date.Months -1 today

        end =
            today
    in
    mkPresetFromDates "Past Month" start end PastMonth


{-| An opaque function for the default preset "Past Year"
-}
presetPastYear : Date -> Preset
presetPastYear today =
    let
        start =
            Date.add Date.Years -1 today

        end =
            today
    in
    mkPresetFromDates "Past Year" start end PastYear


{-| An opaque function for the default preset "One Week From Today"
-}
presetOneWeekFromToday : Date -> Preset
presetOneWeekFromToday today =
    let
        start =
            Date.add Date.Weeks 1 today

        end =
            Date.add Date.Weeks 1 today
    in
    mkPresetFromDates "One Week From Today" start end (WeeksFromToday 1)


{-| An opaque function for the default preset "Two Weeks From Today"
-}
presetTwoWeeksFromToday : Date -> Preset
presetTwoWeeksFromToday today =
    let
        start =
            Date.add Date.Weeks 2 today

        end =
            Date.add Date.Weeks 2 today
    in
    mkPresetFromDates "Two Week From Today" start end (WeeksFromToday 2)


{-| An opaque function for the default preset "One Month From Today"
-}
presetOneMonthFromToday : Date -> Preset
presetOneMonthFromToday today =
    let
        start =
            Date.add Date.Months 1 today

        end =
            Date.add Date.Months 1 today
    in
    mkPresetFromDates "One Month From Today" start end (MonthsFromToday 1)


{-| An opaque function for the default preset "Three Months From Today"
-}
presetThreeMonthsFromToday : Date -> Preset
presetThreeMonthsFromToday today =
    let
        start =
            Date.add Date.Months 3 today

        end =
            Date.add Date.Months 3 today
    in
    mkPresetFromDates "Three Months From Today" start end (MonthsFromToday 3)


{-| An opaque function for the default preset "Six Months From Today"
-}
presetSixMonthsFromToday : Date -> Preset
presetSixMonthsFromToday today =
    let
        start =
            Date.add Date.Months 6 today

        end =
            Date.add Date.Months 6 today
    in
    mkPresetFromDates "Six Months From Today" start end (MonthsFromToday 6)


{-| An opaque function for the default preset "Six Months From Today"
-}
presetEndOfTime : Preset
presetEndOfTime =
    let
        start =
            Date.fromCalendarDate 9999 Dec 31

        end =
            Date.fromCalendarDate 9999 Dec 31
    in
    mkPresetFromDates "End Of Time" start end EndOfTime


{-| A record of default settings for the daterangepicker.
-}
defaultSettings : Settings
defaultSettings =
    { placeholder = "Select a date range"
    , inputName = Nothing
    , inputId = Nothing
    , inputIcon = Nothing
    , inputAttributes = []
    , presetOptions = defaultPresetOptions
    , restrictedDateRange = Off
    , formatDateRange = formatDateRange
    , calendarDisplay = FullCalendar
    , inputView = Nothing
    , disableRange = False
    }


{-| A record of default preset options for the daterangepicker.
-}
defaultPresetOptions : PresetOptions
defaultPresetOptions =
    { presetOption = DefaultPresets
    , presetSettings = []
    , presets = []
    }


{-| The default initial state of the DateRangePicker. You must execute
the returned command in order to set the current date and for the
daterangepicker to behave correctly.
-}
init : ( DateRangePicker, Cmd Msg )
init =
    ( DateRangePicker initModel
    , initCmd
    )


{-| The opaque inital model used within the inital state.
-}
initModel : Model
initModel =
    { today = initDate
    , inputText = Nothing
    , open = False
    , dateRange = Nothing
    , startDate = Nothing
    , endDate = Nothing
    , hoveredDate = Nothing
    , showPresets = False
    , presets = []
    , enabledDateRange = Nothing
    , settings = defaultSettings
    , calendarRange = prepareCalendarRange FullCalendar initDate
    , isMouseDown = False
    , isShiftDown = False
    , selectedPreset = NoneSelected
    , terminationCounter = 10
    }


{-| The opaque initial command to get the current date, used within the
initial state.
-}
initCmd : Cmd Msg
initCmd =
    Task.perform InitCurrentDate Date.today


{-| The daterangepicker update function.
-}
update : Msg -> DateRangePicker -> ( DateRangePicker, Cmd Msg )
update msg (DateRangePicker ({ settings } as model)) =
    let
        ( updatedModel, cmds ) =
            case msg of
                InitCurrentDate date ->
                    let
                        presets =
                            mkPresets settings date

                        enabledDateRange =
                            mkEnabledDateRangeFromRestrictedDateRange settings.restrictedDateRange date

                        newModel =
                            { model
                                | today = fromCalendarDate (year date) (month date) (day date)
                                , presets = presets
                                , enabledDateRange = enabledDateRange
                                , calendarRange = prepareCalendarRange settings.calendarDisplay date
                            }

                        newDateRange =
                            Maybe.map (\x -> getNewDateRange newModel x) model.dateRange
                    in
                    ( { newModel | dateRange = newDateRange }, Cmd.none )

                PrevCalendarRange ->
                    let
                        prevCalendarRange =
                            prepareCalendarRange model.settings.calendarDisplay <|
                                case model.settings.calendarDisplay of
                                    FullCalendar ->
                                        Date.add Date.Years -1 model.calendarRange.start

                                    ThreeMonths ->
                                        Date.add Date.Months -3 model.calendarRange.start

                                    TwoMonths ->
                                        Date.add Date.Months -2 model.calendarRange.start

                                    OneMonth ->
                                        Date.add Date.Months -1 model.calendarRange.start
                    in
                    ( { model | calendarRange = prevCalendarRange }, Cmd.none )

                NextCalendarRange ->
                    let
                        nextCalendarRange =
                            prepareCalendarRange model.settings.calendarDisplay <|
                                case model.settings.calendarDisplay of
                                    FullCalendar ->
                                        Date.add Date.Years 1 model.calendarRange.start

                                    ThreeMonths ->
                                        Date.add Date.Months 3 model.calendarRange.start

                                    TwoMonths ->
                                        Date.add Date.Months 2 model.calendarRange.start

                                    OneMonth ->
                                        Date.add Date.Months 1 model.calendarRange.start
                    in
                    ( { model | calendarRange = nextCalendarRange }, Cmd.none )

                Done ->
                    ( updateDateRange { model | open = False }, Cmd.none )

                ToggleDateRangePicker ->
                    let
                        ( newCalendarRange, startDate, endDate ) =
                            case model.dateRange of
                                Just a ->
                                    ( prepareCalendarRange model.settings.calendarDisplay a.start, Just a.start, Just a.end )

                                Nothing ->
                                    ( model.calendarRange, model.startDate, model.endDate )
                    in
                    ( { model
                        | open = not model.open
                        , calendarRange = newCalendarRange
                        , startDate = startDate
                        , endDate = endDate
                      }
                    , Cmd.none
                    )

                StartSelection date ->
                    ( setStart date { model | isMouseDown = True }
                        |> setSelectedPreset NoneSelected
                    , Cmd.none
                    )

                EndSelection date ->
                    ( setEnd date { model | isMouseDown = False }
                        |> setSelectedPreset NoneSelected
                        |> updateDateRange
                    , Cmd.none
                    )

                SelectSingleDate date ->
                    ( setStart date model
                        |> setEnd (Just date)
                        |> updateDateRange
                        |> setSelectedPreset NoneSelected
                    , Cmd.none
                    )

                SelectPresetOption preset ->
                    ( selectDateRange preset.dateRange model
                        |> setSelectedPreset preset.presetDateOption
                    , Cmd.none
                    )

                SelectDateRange dateRange ->
                    ( selectDateRange dateRange model
                        |> setSelectedPreset NoneSelected
                    , Cmd.none
                    )

                StartSelectionOnShift s ->
                    if s == "Shift" then
                        case Maybe.map2 dateGreaterThanOrEqualTo model.hoveredDate model.startDate of
                            Just True ->
                                ( { model | isShiftDown = True, endDate = Nothing }
                                , Cmd.none
                                )

                            Just False ->
                                ( { model | isShiftDown = True, startDate = model.endDate, endDate = Nothing }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model | isShiftDown = True, endDate = Nothing }
                                , Cmd.none
                                )

                    else
                        ( model, Cmd.none )

                CancelShift s ->
                    if s == "Shift" then
                        ( { model
                            | isShiftDown = False
                            , terminationCounter = 10
                            , endDate = Maybe.map .end model.dateRange
                            , startDate = Maybe.map .start model.dateRange
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                UpdateCounter ->
                    ( { model
                        | terminationCounter =
                            if model.terminationCounter >= 2 then
                                model.terminationCounter

                            else
                                model.terminationCounter + 1
                      }
                    , Cmd.none
                    )

                TerminateBadState ->
                    if model.terminationCounter < 0 then
                        ( { model
                            | isShiftDown = False
                            , isMouseDown = False
                            , endDate = Maybe.map .end model.dateRange
                            , startDate = Maybe.map .start model.dateRange
                            , terminationCounter = 10
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | terminationCounter = model.terminationCounter - 1 }, Cmd.none )

                Clear ->
                    ( { model
                        | dateRange = Nothing
                        , startDate = Nothing
                        , endDate = Nothing
                        , hoveredDate = Nothing
                        , showPresets = False
                      }
                        |> setSelectedPreset NoneSelected
                    , initCmd
                    )

                HoverDay date ->
                    ( { model | hoveredDate = Just date }, Cmd.none )

                DoNothing ->
                    ( model, Cmd.none )
    in
    ( DateRangePicker <| updateInputText updatedModel, cmds )


{-| Expose if the daterange picker is open
-}
isOpen : DateRangePicker -> Bool
isOpen (DateRangePicker model) =
    model.open


{-| Sets the the open state of the DateRangePicker
-}
setOpen : Bool -> DateRangePicker -> DateRangePicker
setOpen open (DateRangePicker model) =
    DateRangePicker { model | open = open }


{-| Expose the current selected daterange.
-}
getDateRange : DateRangePicker -> Maybe DateRange
getDateRange (DateRangePicker model) =
    model.dateRange


{-| Expose the currently selected preset.
-}
getSelectedPreset : DateRangePicker -> PresetType
getSelectedPreset (DateRangePicker model) =
    model.selectedPreset


{-| Expose the current presets.
-}
getPresets : DateRangePicker -> List Preset
getPresets (DateRangePicker model) =
    model.presets


{-| Expose the min date in the enabled date range.
-}
getMinDate : DateRangePicker -> Maybe Date
getMinDate (DateRangePicker model) =
    Maybe.andThen .start model.enabledDateRange


{-| Expose the max date in the enabled date range.
-}
getMaxDate : DateRangePicker -> Maybe Date
getMaxDate (DateRangePicker model) =
    Maybe.andThen .end model.enabledDateRange


{-| Expose today's date.
-}
getToday : DateRangePicker -> Date
getToday (DateRangePicker model) =
    model.today


{-| Sets the current daterange for the daterangepicker.
-}
setDateRange : Maybe DateRange -> DateRangePicker -> DateRangePicker
setDateRange dateRange (DateRangePicker model) =
    let
        newDateRange =
            Maybe.map (\x -> getNewDateRange model x) dateRange
    in
    DateRangePicker ({ model | dateRange = newDateRange } |> updateInputText)


{-| Sets the date range formatter for the daterangepicker.
-}
setDateRangeFormat : (DateRange -> String) -> DateRangePicker -> DateRangePicker
setDateRangeFormat dateRangeFormat (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | formatDateRange = dateRangeFormat }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets the placeholder for the daterangepicker.
-}
setPlaceholder : String -> DateRangePicker -> DateRangePicker
setPlaceholder placeholder (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | placeholder = placeholder }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets the name for the daterangepicker.
-}
setInputName : String -> DateRangePicker -> DateRangePicker
setInputName inputName (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputName = Just inputName }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets the inputText for the daterangepicker.
-}
setInputText : Maybe String -> DateRangePicker -> DateRangePicker
setInputText inputText (DateRangePicker model) =
    DateRangePicker { model | inputText = inputText }


{-| Sets the id for the daterangepicker.
-}
setInputId : String -> DateRangePicker -> DateRangePicker
setInputId inputId (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputId = Just inputId }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets the input attributes for the daterangepicker.
-}
setInputAttributes : List (Html.Attribute Msg) -> DateRangePicker -> DateRangePicker
setInputAttributes inputAttributes (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputAttributes = inputAttributes }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets the input icon for the daterangepicker.
-}
setInputIcon : Html Msg -> DateRangePicker -> DateRangePicker
setInputIcon inputIcon (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputIcon = Just inputIcon }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets the preset options for the daterangepicker.
-}
setPresetOptions : PresetOptions -> DateRangePicker -> DateRangePicker
setPresetOptions presetOptions (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | presetOptions = presetOptions }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets the restricted date range for the daterangepicker.
-}
setRestrictedDateRange : RestrictedDateRange -> DateRangePicker -> DateRangePicker
setRestrictedDateRange restrictedDateRange (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | restrictedDateRange = restrictedDateRange }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets whether range selection is disabled for the daterangepicker.
-}
setDisableRange : Bool -> DateRangePicker -> DateRangePicker
setDisableRange isDisabled (DateRangePicker ({ settings } as model)) =
    let
        newSettings =
            { settings | disableRange = isDisabled }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets the CalendarDisplay for the daterangepicker
-}
setCalendarDisplay : CalendarDisplay -> DateRangePicker -> DateRangePicker
setCalendarDisplay calendarDisplay (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | calendarDisplay = calendarDisplay }

        newCalendarRange =
            prepareCalendarRange calendarDisplay <|
                case model.dateRange of
                    Nothing ->
                        model.today

                    Just { end } ->
                        end
    in
    DateRangePicker { model | settings = newSettings, calendarRange = newCalendarRange }


{-| Sets the inputView function for the daterangepicker
-}
setInputView : Maybe (String -> List (Html Msg)) -> DateRangePicker -> DateRangePicker
setInputView fn (DateRangePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputView = fn }
    in
    DateRangePicker { model | settings = newSettings }


{-| Sets the settings for the daterange picker
-}
setSettings : Settings -> DateRangePicker -> DateRangePicker
setSettings settings (DateRangePicker model) =
    DateRangePicker { model | settings = settings }


{-| Subscribes to a mouse click
-}
subscriptions : DateRangePicker -> Sub Msg
subscriptions (DateRangePicker model) =
    let
        shiftSubs =
            if model.isShiftDown then
                [ Browser.Events.onKeyUp (Json.field "key" Json.string |> Json.map CancelShift)
                , Browser.Events.onVisibilityChange (CancelShift "Shift" |> always)
                , Browser.Events.onKeyDown (Json.field "key" Json.string |> Json.map (always UpdateCounter))
                , Time.every 100 (always TerminateBadState)
                ]

            else
                [ Browser.Events.onKeyDown (Json.field "key" Json.string |> Json.map StartSelectionOnShift)
                ]

        mouseSubs =
            if model.isMouseDown then
                [ Browser.Events.onMouseUp (EndSelection model.hoveredDate |> Json.succeed)
                , Browser.Events.onVisibilityChange (EndSelection model.hoveredDate |> always)
                ]

            else
                []
    in
    if model.open then
        Browser.Events.onClick (Json.succeed Done) :: List.concat [ shiftSubs, mouseSubs ] |> Sub.batch

    else
        Sub.none


{-| The daterange picker view. The date range passed is whatever date range it should treat as selected.
-}
view : DateRangePicker -> Html Msg
view (DateRangePicker ({ open, settings } as model)) =
    let
        potentialInputId =
            settings.inputId
                |> Maybe.map Attrs.id
                |> (List.singleton >> List.filterMap identity)

        dateInputAttrs =
            List.concat
                [ [ Attrs.name <| Maybe.withDefault "" settings.inputName
                  , onClickNoDefault ToggleDateRangePicker
                  , Attrs.class (classPrefix ++ "date-input")
                  ]
                , settings.inputAttributes
                , potentialInputId
                ]

        dateInputText =
            Maybe.withDefault settings.placeholder model.inputText

        inputView =
            case settings.inputView of
                Just fn ->
                    div dateInputAttrs <| fn dateInputText

                Nothing ->
                    let
                        icon =
                            Maybe.withDefault (i [] []) settings.inputIcon
                    in
                    div
                        dateInputAttrs
                        [ text dateInputText
                        , icon
                        ]
    in
    div [ Attrs.class (classPrefix ++ "container") ]
        [ inputView
        , if open then
            dateRangePicker model

          else
            text ""
        ]


{-| An opaque function to create the daterange picker view.
-}
dateRangePicker : Model -> Html Msg
dateRangePicker model =
    let
        calendarDisplayClass =
            classPrefix ++ calendarDisplayToClassStr model.settings.calendarDisplay
    in
    div
        [ Attrs.classList
            [ ( classPrefix ++ "wrapper", True )
            , ( classPrefix ++ "calendar-tab", True )
            , ( classPrefix ++ "box-shadow", True )
            , ( calendarDisplayClass, True )
            ]
        , onClickNoDefault DoNothing
        ]
        [ renderCalendar model
        , renderPresets model
        ]


{-| An opaque function that prints the daterangepicker calendar.
-}
renderCalendar : Model -> Html Msg
renderCalendar model =
    let
        calendarDisplayClass =
            classPrefix ++ calendarDisplayToClassStr model.settings.calendarDisplay

        body =
            table [ Attrs.class (classPrefix ++ "body--container") ] <|
                case model.settings.calendarDisplay of
                    FullCalendar ->
                        renderFullCalendarBody model

                    ThreeMonths ->
                        renderThreeMonthsBody model

                    TwoMonths ->
                        renderTwoMonthsBody model

                    OneMonth ->
                        renderOneMonthBody model
    in
    div
        [ Attrs.class (classPrefix ++ "calendar")
        , Attrs.class calendarDisplayClass
        ]
        [ renderDateRangePickerHeader model
        , body
        ]


{-| An opaque function that gets the Html Msg for the presets of the daterange picker.
-}
renderPresets : Model -> Html Msg
renderPresets model =
    div [ Attrs.class (classPrefix ++ "presets") ] <|
        if List.length model.presets > 0 then
            List.concat
                [ [ div [ Attrs.class (classPrefix ++ "presets-header") ] [ text "Presets" ] ]
                , List.map (renderPreset model) model.presets
                , [ div [ Attrs.class (classPrefix ++ "buttons") ]
                        [ div [ Attrs.class (classPrefix ++ "btn"), onClickNoDefault Clear ] [ text "Clear" ]
                        , div [ Attrs.class (classPrefix ++ "btn"), onClickNoDefault Done ] [ text "Done" ]
                        ]
                  ]
                ]

        else
            noPresets


{-| An opaque function that gets the Html Msg for a given preset.
-}
renderPreset : Model -> Preset -> Html Msg
renderPreset model preset =
    let
        isDisabledPreset =
            isDisabledDate model.enabledDateRange preset.dateRange.start
                && isDisabledDate model.enabledDateRange preset.dateRange.end

        setDateRange_ =
            if isDisabledPreset then
                onClickNoDefault DoNothing

            else
                onClickNoDefault <| SelectPresetOption preset
    in
    div
        [ Attrs.classList
            [ ( classPrefix ++ "preset", True )
            , ( classPrefix ++ "disabled", isDisabledPreset )
            ]
        , setDateRange_
        ]
        [ span [ Attrs.class (classPrefix ++ "preset-name") ] [ text preset.name ]
        ]


{-| An opaque function that gets the year header Html Msg for the calendar.
-}
renderDateRangePickerHeader : Model -> Html Msg
renderDateRangePickerHeader model =
    let
        ( start, end ) =
            ( model.calendarRange.start, model.calendarRange.end )

        isDisabledDateRange =
            isDisabledDate model.enabledDateRange start
                && isDisabledDate model.enabledDateRange end

        setRange =
            if isDisabledDateRange then
                DoNothing

            else
                SelectDateRange <| mkDateRange start end
    in
    div [ Attrs.class (classPrefix ++ "yr-label-wrapper") ]
        [ div [ Attrs.classList [ ( classPrefix ++ "range-btn", True ), ( classPrefix ++ "range-prev", True ) ], onClickNoDefault PrevCalendarRange ] [ text "❮" ]
        , div
            [ Attrs.classList
                [ ( classPrefix ++ "range-btn", True )
                , ( classPrefix ++ "range-label", True )
                , ( classPrefix ++ "disabled", isDisabledDateRange )
                ]
            , onClickNoDefault setRange
            ]
            [ text model.calendarRange.name ]
        , div [ Attrs.classList [ ( classPrefix ++ "range-btn", True ), ( classPrefix ++ "range-next", True ) ], onClickNoDefault NextCalendarRange ] [ text "❯" ]
        ]


{-| An opaque function that gets the List (Html Msg) for the body of the dateRangePicker for FullCalendarYear
-}
renderFullCalendarBody : Model -> List (Html Msg)
renderFullCalendarBody model =
    let
        quarters =
            chunksOfLeft 3 model.calendarRange.months
    in
    List.map (renderQuarter False model) quarters


{-| An opaque function that gets the List (Html Msg) for the body of the dateRangePicker for ThreeMonths
-}
renderThreeMonthsBody : Model -> List (Html Msg)
renderThreeMonthsBody model =
    [ div [ Attrs.classList [ ( classPrefix ++ "months-row", True ), ( classPrefix ++ "larger-months", True ) ] ]
        [ renderQuarter True model model.calendarRange.months ]
    ]


{-| An opaque function that gets the List (Html Msg) for the body of the dateRangePicker for TwoMonths
-}
renderTwoMonthsBody : Model -> List (Html Msg)
renderTwoMonthsBody model =
    [ div [ Attrs.classList [ ( classPrefix ++ "months-row", True ), ( classPrefix ++ "larger-months", True ) ] ] <|
        List.map (renderMonth True model) model.calendarRange.months
    ]


{-| An opaque function that gets the List (Html Msg) for the body of the dateRangePicker for OneMonth
-}
renderOneMonthBody : Model -> List (Html Msg)
renderOneMonthBody model =
    [ div [ Attrs.classList [ ( classPrefix ++ "months-row", True ), ( classPrefix ++ "larger-months", True ) ] ] <|
        List.map (renderMonth True model) model.calendarRange.months
    ]


{-| An opaque function that gets the Html Msg for a given Quarter.
-}
renderQuarter : Bool -> Model -> Months -> Html Msg
renderQuarter listWeeksPastFirstQ model months =
    let
        firstMonthOfQtr =
            List.head months

        lastMonthOfQtr =
            List.head <|
                List.reverse months
    in
    case ( firstMonthOfQtr, lastMonthOfQtr ) of
        ( Just firstMonth, Just lastMonth ) ->
            let
                startOfQtr =
                    List.head firstMonth

                endOfQtr =
                    List.head <|
                        List.reverse lastMonth

                qtrElement =
                    case ( startOfQtr, endOfQtr ) of
                        ( Just start, Just end ) ->
                            let
                                isDisabledQtr =
                                    isDisabledDate model.enabledDateRange start
                                        && isDisabledDate model.enabledDateRange end

                                setQtrDateRange =
                                    if isDisabledQtr then
                                        onClickNoDefault DoNothing

                                    else
                                        onClickNoDefault <| SelectDateRange <| mkDateRange start end

                                qtrLabel =
                                    td
                                        [ Attrs.classList
                                            [ ( classPrefix ++ "qtr-label", True )
                                            , ( classPrefix ++ "disabled", isDisabledQtr )
                                            ]
                                        , setQtrDateRange
                                        ]
                                        [ text <| "Q" ++ (String.fromInt <| Date.quarter start) ]
                            in
                            tr [ Attrs.class (classPrefix ++ "qtr-row") ] <|
                                List.concat
                                    [ [ qtrLabel ]
                                    , List.map (renderMonth (Date.quarter start == 1 || listWeeksPastFirstQ) model) months
                                    ]

                        ( _, _ ) ->
                            text ""
            in
            qtrElement

        ( _, _ ) ->
            text ""


{-| An opaque function that gets the Html Msg for a given month of the calendar.
-}
renderMonth : Bool -> Model -> List Date -> Html Msg
renderMonth includeWeeks model m =
    let
        h =
            List.head m
    in
    case h of
        Just a ->
            let
                days =
                    List.concat
                        [ padMonthLeft a
                        , List.map (renderDay model) m
                        ]

                startOfMonth_ =
                    startOfMonth a

                endOfMonth_ =
                    endOfMonth a

                isDisabledMonth =
                    isDisabledDate model.enabledDateRange startOfMonth_
                        && isDisabledDate model.enabledDateRange endOfMonth_

                setMonthDateRange =
                    if isDisabledMonth then
                        onClickNoDefault DoNothing

                    else
                        onClickNoDefault <| SelectDateRange <| mkDateRange (startOfMonth a) (endOfMonth a)

                listOfWeeks =
                    if includeWeeks then
                        tr [ Attrs.class (classPrefix ++ "week-days") ] renderDaysOfWeek

                    else
                        text ""

                header =
                    thead
                        []
                        [ listOfWeeks
                        , tr
                            [ Attrs.classList
                                [ ( classPrefix ++ "month-label", True )
                                , ( classPrefix ++ "disabled", isDisabledMonth )
                                ]
                            , setMonthDateRange
                            ]
                            [ text <|
                                formatMonth <|
                                    month a
                            ]
                        ]
            in
            table [ Attrs.class (classPrefix ++ "month-wrapper") ] <|
                [ header
                , List.concat [ days, padMonthRight (42 - List.length days) ]
                    |> tbody [ Attrs.class (classPrefix ++ "month") ]
                ]

        _ ->
            text ""


{-| An opaque function that gets the Html Msg for a Day.
-}
renderDay : Model -> Date -> Html Msg
renderDay model date =
    let
        dayOfWeek =
            Date.weekday date |> dayToInt

        dayOfMonth =
            day date

        isDisabledDate_ =
            isDisabledDate model.enabledDateRange date

        isSelectedDateRange_ =
            isSelectedDateRange model date

        isHoveredDateRange_ =
            isHoveredDateRange model date
                && not isDisabledDate_
                && not isSelectedDateRange_

        isToday_ =
            isToday model date

        isStart_ =
            isStart model date

        isEnd_ =
            isEnd model date

        setDate_ =
            case ( isDisabledDate_, model.settings.disableRange, model.isShiftDown ) of
                ( True, _, _ ) ->
                    onClickNoDefault DoNothing

                ( False, True, _ ) ->
                    SelectSingleDate date |> onClickNoDefault

                ( False, False, True ) ->
                    Just date |> EndSelection |> onClickNoDefault

                ( False, False, False ) ->
                    if model.isMouseDown then
                        Just date |> EndSelection |> onClickNoDefault

                    else
                        StartSelection date |> DateRangePicker.Helper.mouseDownNoDefault
    in
    td
        [ Attrs.classList
            [ ( classPrefix ++ "day", True )
            , ( classPrefix ++ "today", isToday_ )
            , ( classPrefix ++ "selected-range", isSelectedDateRange_ && not isStart_ && not isEnd_ )
            , ( classPrefix ++ "hovered-range", isHoveredDateRange_ && (model.isMouseDown || model.isShiftDown) )
            , ( classPrefix ++ "disabled", isDisabledDate_ )
            , ( classPrefix ++ "start", isStart_ )
            , ( classPrefix ++ "end", isEnd_ )
            , ( "border-b", dayOfMonth - dayOfWeek < 30 )
            , ( "border-r", dayOfWeek /= 6 )
            ]
        , setDate_
        , Html.Events.onMouseOver <| HoverDay date
        ]
        [ td [ Attrs.class (classPrefix ++ "bubble") ]
            [ text <|
                String.fromInt dayOfMonth
            ]
        ]


{-| An opaque function the check if the given date is in the selected range.
-}
isSelectedDateRange : Model -> Date -> Bool
isSelectedDateRange model date =
    case ( model.startDate, model.endDate ) of
        ( Just s, Just e ) ->
            mkDateRange s e |> inRange date

        _ ->
            False


{-| An opaque function that checks if the given date is between the start date and hovered date.
-}
isHoveredDateRange : Model -> Date -> Bool
isHoveredDateRange model date =
    case ( model.startDate, model.hoveredDate, model.endDate ) of
        ( Just s, Just h, Nothing ) ->
            inRange date (mkDateRange s h) || inRange date (mkDateRange h s)

        _ ->
            False


{-| An opaque function that checks if the passed in date is equal
to the model's startDate
-}
isStart : Model -> Date -> Bool
isStart model date =
    case ( model.startDate, model.endDate, model.hoveredDate ) of
        ( Just start, Nothing, Just hovered ) ->
            dateEqualTo start date && dateLessThanOrEqualTo date hovered || dateEqualTo hovered date && dateLessThanOrEqualTo date start

        ( Just start, Just _, _ ) ->
            dateEqualTo start date

        ( Just start, Nothing, Nothing ) ->
            dateEqualTo start date

        ( Nothing, _, _ ) ->
            False


{-| An opaque function that checks if the passed in date is equal
to the model's endDate
-}
isEnd : Model -> Date -> Bool
isEnd model date =
    case ( model.startDate, model.endDate, model.hoveredDate ) of
        ( Just start, Nothing, Just hovered ) ->
            dateEqualTo start date && dateGreaterThanOrEqualTo date hovered || dateEqualTo hovered date && dateGreaterThanOrEqualTo date start

        ( Just _, Just end, _ ) ->
            dateEqualTo end date

        ( Just start, Nothing, Nothing ) ->
            dateEqualTo start date

        ( Nothing, _, _ ) ->
            False


{-| An opaque function that checks if the passed in date is today.
-}
isToday : Model -> Date -> Bool
isToday model date =
    dateEqualTo date model.today


{-| An opaque function that gets the new date range from a selected date range
and the enabled dates of the date picker.
-}
getNewDateRange : Model -> DateRange -> DateRange
getNewDateRange model dateRange =
    case model.enabledDateRange of
        Just dr ->
            case ( dr.start, dr.end ) of
                ( Just start, Just end ) ->
                    let
                        newStart =
                            if inRange dateRange.start <| mkDateRange start end then
                                dateRange.start

                            else
                                start

                        newEnd =
                            if inRange dateRange.end <| mkDateRange start end then
                                dateRange.end

                            else
                                end
                    in
                    mkDateRange newStart newEnd

                ( Just start, Nothing ) ->
                    let
                        newStart =
                            if dateGreaterThanOrEqualTo dateRange.start start then
                                dateRange.start

                            else
                                start

                        newEnd =
                            if dateGreaterThanOrEqualTo dateRange.end start then
                                dateRange.end

                            else
                                start
                    in
                    mkDateRange newStart newEnd

                ( Nothing, Just end ) ->
                    let
                        newStart =
                            if dateLessThanOrEqualTo dateRange.start end then
                                dateRange.start

                            else
                                end

                        newEnd =
                            if dateLessThanOrEqualTo dateRange.end end then
                                dateRange.end

                            else
                                end
                    in
                    mkDateRange newStart newEnd

                ( Nothing, Nothing ) ->
                    dateRange

        Nothing ->
            dateRange


{-| An opaque function that updates the inputText based on the
model's selected dateRange
-}
updateInputText : Model -> Model
updateInputText model =
    case model.dateRange of
        Just a ->
            { model | inputText = Just <| model.settings.formatDateRange a }

        Nothing ->
            { model | inputText = Nothing }


{-| A function that formats a daterange to a string.
-}
formatDateRange : DateRange -> String
formatDateRange dateRange =
    if dateRange.start /= dateRange.end then
        String.concat
            [ formatDate dateRange.start
            , " - "
            , formatDate dateRange.end
            ]

    else
        formatDate dateRange.start


{-| An opaque function that sets the start date.
-}
setStart : Date -> Model -> Model
setStart date model =
    { model | startDate = Just date, endDate = Nothing }


{-| An opaque function that sets the end date with some conditions.
-}
setEnd : Maybe Date -> Model -> Model
setEnd date model =
    if Maybe.map2 dateGreaterThanOrEqualTo date model.startDate |> Maybe.withDefault False then
        { model | endDate = date }

    else
        { model | endDate = model.startDate, startDate = date }


{-| An opaque function that sets the selected preset option if a preset was used to select the daterange.
-}
setSelectedPreset : PresetType -> Model -> Model
setSelectedPreset preset model =
    { model | selectedPreset = preset }


{-| An opaque function that sets daterange.
-}
selectDateRange : DateRange -> Model -> Model
selectDateRange dateRange model =
    let
        calendarDateRange =
            mkDateRange model.calendarRange.start model.calendarRange.end

        newCalendarRange =
            if inRange dateRange.start calendarDateRange || inRange dateRange.end calendarDateRange then
                model.calendarRange

            else
                prepareCalendarRange model.settings.calendarDisplay dateRange.start
    in
    { model | startDate = Nothing, endDate = Nothing, calendarRange = newCalendarRange }
        |> setStart dateRange.start
        |> setEnd (Just dateRange.end)
        |> updateDateRange


{-| An opaque function that updates the dateRange based on values
of startDate and endDate
-}
updateDateRange : Model -> Model
updateDateRange ({ startDate, endDate } as model) =
    case ( startDate, endDate ) of
        ( Just s, Just e ) ->
            if dateLessThanOrEqualTo s e then
                { model | dateRange = Just <| mkDateRange s e }

            else
                { model | dateRange = Just <| mkDateRange e s }

        ( Just s, Nothing ) ->
            { model | dateRange = Just <| mkDateRange s s }

        ( Nothing, _ ) ->
            model


{-| A function that creates a DateRange by taking in two dates (start and end).
This function assumes that start <= end
-}
mkDateRange : Date -> Date -> DateRange
mkDateRange =
    DateRangePicker.Helper.mkDateRange


{-| A function to check if a given date is within a
given dateRange.
-}
inRange : Date -> DateRange -> Bool
inRange =
    DateRangePicker.Helper.inRange


{-| A function that returns the number of years as a whole number in the daterange
-}
yearsInRange : DateRange -> Int
yearsInRange =
    DateRangePicker.Helper.yearsInRange


{-| A function that returns the number of months as a whole number in the daterange
-}
monthsInRange : DateRange -> Int
monthsInRange =
    DateRangePicker.Helper.monthsInRange


{-| A function that returns the number of weeks as a whole number in the daterange
-}
weeksInRange : DateRange -> Int
weeksInRange =
    DateRangePicker.Helper.weeksInRange


{-| A function that returns the number of days as a whole number in the daterange
-}
daysInRange : DateRange -> Int
daysInRange =
    DateRangePicker.Helper.daysInRange


{-| A function that takes a Date and returns the date representing the first date of the quarter that the passed in date belongs to.
-}
startOfQuarter : Date -> Date
startOfQuarter =
    DateRangePicker.Helper.startOfQuarter


{-| A function that takes a Date and returns the date representing the first date of the quarter that the passed in date belongs to.
-}
endOfQuarter : Date -> Date
endOfQuarter =
    DateRangePicker.Helper.endOfQuarter


{-| An opaque function that prepares the full year based on the given date.
-}
prepareCalendarRange : CalendarDisplay -> Date -> CalendarRange
prepareCalendarRange calendarDisplay date =
    let
        yr =
            Date.year date

        ( start, end ) =
            case calendarDisplay of
                FullCalendar ->
                    ( fromCalendarDate yr Jan 1
                    , fromCalendarDate yr Dec 31
                    )

                ThreeMonths ->
                    ( startOfQuarter date, endOfQuarter date )

                TwoMonths ->
                    ( startOfMonth date, endOfMonth <| Date.add Date.Months 1 date )

                OneMonth ->
                    ( startOfMonth date, endOfMonth date )

        dates =
            Date.range Date.Day 1 start (Date.add Date.Days 1 end)

        name =
            case calendarDisplay of
                FullCalendar ->
                    String.fromInt yr

                ThreeMonths ->
                    String.join " - "
                        [ String.join " " [ monthAbbr <| Date.month start, String.fromInt yr ]
                        , String.join " " [ monthAbbr <| Date.month end, String.fromInt yr ]
                        ]

                TwoMonths ->
                    String.join " - "
                        [ String.join " " [ monthAbbr <| Date.month start, String.fromInt yr ]
                        , String.join " " [ monthAbbr <| Date.month end, String.fromInt yr ]
                        ]

                OneMonth ->
                    String.join " " [ formatMonth <| Date.month start, String.fromInt yr ]

        months =
            List.map (\x -> Tuple.first x :: Tuple.second x) <|
                LE.groupWhile (\x y -> Date.month x == Date.month y) dates
    in
    CalendarRange name start end months


{-| A function that formats a Month into the abbreviated string.

  - Ex. Jan -> "Jan"

-}
monthAbbr : Month -> String
monthAbbr =
    DateRangePicker.Helper.monthAbbr


{-| An opaque function that makes the EnabledDateRange from settings.


## EnabledDateRange is Nothing if RestrictedDateRange is Off

-}
mkEnabledDateRangeFromRestrictedDateRange : RestrictedDateRange -> Date -> Maybe EnabledDateRange
mkEnabledDateRangeFromRestrictedDateRange restrictedDateRange today =
    case restrictedDateRange of
        Off ->
            mkEnabledDateRange Nothing Nothing

        ToPresent ->
            mkEnabledDateRange Nothing (Just today)

        FromPresent ->
            mkEnabledDateRange (Just today) Nothing

        Past ->
            let
                yesterday =
                    Date.add Date.Days -1 today
            in
            mkEnabledDateRange Nothing (Just yesterday)

        Future ->
            let
                tomorrow =
                    Date.add Date.Days 1 today
            in
            mkEnabledDateRange (Just tomorrow) Nothing

        Between start end ->
            mkEnabledDateRange (Just start) (Just end)

        To date ->
            mkEnabledDateRange Nothing (Just date)

        From date ->
            mkEnabledDateRange (Just date) Nothing


{-| An opaque function that makes an EnabledDateRange from two Maybe Dates
-}
mkEnabledDateRange : Maybe Date -> Maybe Date -> Maybe EnabledDateRange
mkEnabledDateRange start end =
    case ( start, end ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( _, _ ) ->
            Just <|
                { start = start
                , end = end
                }


{-| An opaque function that gets the class string for the CalendarDisplay
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


classPrefix : String
classPrefix =
    "elm-fancy-daterangepicker--"
