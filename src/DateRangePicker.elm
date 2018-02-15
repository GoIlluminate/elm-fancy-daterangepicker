module DateRangePicker
    exposing
        ( Msg
        , Settings
        , PresetOptions
        , PresetOption(..)
        , PresetInterval(..)
        , PresetRelativeToToday(..)
        , PresetSetting
        , Preset
        , RestrictedDateRange(..)
        , DateRange
        , DateRangePicker
        , mkPresetFromDateRange
        , mkPresetFromDates
        , mkDateRange
        , defaultPresets
        , defaultSettings
        , defaultPresetOptions
        , init
        , update
        , isOpen
        , getDateRange
        , setDateRange
        , setSettings
        , view
        )

{-| A customizable daterangepicker component.

@docs Msg, DateRangePicker, DateRange
@docs init, update, isOpen, view, mkDateRange, getDateRange, setDateRange, setSettings


# Settings

@docs Settings, RestrictedDateRange, defaultSettings


## Presets

@docs PresetOptions, PresetOption, Preset, PresetSetting, PresetInterval, PresetRelativeToToday, defaultPresetOptions, defaultPresets, mkPresetFromDateRange, mkPresetFromDates

-}

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, text, table, thead, th, tbody, tr, td, p, h1, input, button, a, i, span)
import Html.Attributes as Attrs exposing (class, colspan, type_, placeholder, value, href)
import Html.Events exposing (onClick, onDoubleClick, on, onBlur, onInput, onFocus, onWithOptions)
import Task
import List.Extra as LE
import DateRangePicker.Date exposing (initDate, mkDate, startOfMonth, endOfMonth, datesInRange, dayToInt, dayFromInt, formatDay, formatDate, formatMonth, daysInMonth, subDays, addDays, subMonths, addMonths, subYears, addYears)
import Json.Decode as Json


{-| An opaque type representing messages that are passed within the DateRangePicker.
-}
type Msg
    = InitCurrentDate Date
    | PrevYear
    | NextYear
    | SetDateRange DateRange
    | SetDate Date
    | DoNothing
    | Focus
    | Blur
    | MouseDown
    | MouseUp
    | Done
    | Reset
    | TogglePresets


{-| The opaque model to be used within the DateRangePicker.
-}
type alias Model =
    { today : Date
    , inputText : Maybe String
    , open : Bool
    , forceOpen : Bool
    , currentYear : FullYear
    , dateRange : Maybe DateRange
    , startDate : Maybe Date
    , endDate : Maybe Date
    , showPresets : Bool
    , presets : List Preset
    , enabledDateRange : Maybe EnabledDateRange
    , settings : Settings
    }


{-| The settings that the DateRangePicker uses.
-}
type alias Settings =
    { placeholder : String
    , inputName : Maybe String
    , inputId : Maybe String
    , inputAttributes : List (Html.Attribute Msg)
    , presetOptions : PresetOptions
    , restrictedDateRange : RestrictedDateRange
    , formatDateRange : DateRange -> String
    }


{-| A type representing your preset options for your date range picker.
-}
type alias PresetOptions =
    { presetOption : PresetOption
    , presetSettings : List PresetSetting
    , presets : List Preset
    }


{-| A type representing which presets to use.

  - *DefaultPresets* = Use the default presets from defaultPresets only.
  - *CustomPresetsFromSettings* = Use only your custom built presets from a list of PresetSetting only.
  - *CustomPresets* = Use only your custom built Presets.
  - *CustomOnly* = Use only your custom build presets build from PresetSetting and your custom presets.
  - *AllPresets* = Use all presets (default, customFromSettings, custom).
  - *NoPresets* = Turn off Presets.

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

  - *name* = The name that you want to give the preset. i.e. "Past Month"
  - *interval* = The interval in which you want to add/subtract the value from today.
  - *presetRelativeToToday* = whether it is a range from [past - present] (ToToday) or [present - future] (FromToday)
  - *value* = the number of your @interval that you are adding/subtracting.


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


{-| A type that represents a preset daterange.

  - *name* = Name of the preset. i.e. "Past Month"
  - *dateRange* = The daterange that is selected when selecting the preset.

-}
type alias Preset =
    { name : String
    , dateRange : DateRange
    }


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
    | Between Date.Date Date.Date
    | To Date.Date
    | From Date.Date


{-| A type representing a date range with a start date and end date.
-}
type alias DateRange =
    { start : Date
    , end : Date
    }


{-| An opaque type representing the enabled dates for the datepicker
-}
type alias EnabledDateRange =
    { start : Maybe Date
    , end : Maybe Date
    }


{-| The DateRangePicker model.
-}
type DateRangePicker
    = DateRangePicker Model


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


{-| An opaque function that makes the EnabledDateRange from settings.


## EnabledDateRange is Nothing if RestrictedDateRange is Off

-}
mkEnabledDateRangeFromSettings : Settings -> Date -> Maybe EnabledDateRange
mkEnabledDateRangeFromSettings settings today =
    case settings.restrictedDateRange of
        Off ->
            mkEnabledDateRange Nothing Nothing

        ToPresent ->
            mkEnabledDateRange Nothing (Just today)

        FromPresent ->
            mkEnabledDateRange (Just today) Nothing

        Past ->
            let
                yesterday =
                    subDays 1 today
            in
                mkEnabledDateRange Nothing (Just yesterday)

        Future ->
            let
                tomorrow =
                    addDays 1 today
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
        ( Just a, Just b ) ->
            Just <|
                { start = Just <| mkDate (year a) (month a) (day a)
                , end = Just <| mkDate (year b) (month b) (day b)
                }

        ( Just a, Nothing ) ->
            Just <|
                { start = Just <| mkDate (year a) (month a) (day a)
                , end = Nothing
                }

        ( Nothing, Just b ) ->
            Just <|
                { start = Nothing
                , end = Just <| mkDate (year b) (month b) (day b)
                }

        ( Nothing, Nothing ) ->
            Nothing


{-| An opaque function to make presets from settings and a date
-}
mkPresets : Settings -> Date -> List Preset
mkPresets settings date =
    let
        presetOptions =
            settings.presetOptions

        defaultPresets_ =
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
                            subDays value today

                        Months ->
                            subMonths value today

                        Years ->
                            subYears value today

        end =
            case presetRelativeToToday of
                FromToday ->
                    case interval of
                        Days ->
                            addDays value today

                        Months ->
                            addMonths value today

                        Years ->
                            addYears value today

                ToToday ->
                    today
    in
        { name = name
        , dateRange = mkDateRange start end
        }


{-| A function that creates a Preset from a name and a dateRange
-}
mkPresetFromDateRange : String -> DateRange -> Preset
mkPresetFromDateRange name dateRange =
    { name = name
    , dateRange = dateRange
    }


{-| A function that creates a Preset from a name, startDate, and endDate
-}
mkPresetFromDates : String -> Date -> Date -> Preset
mkPresetFromDates name start end =
    { name = name
    , dateRange = mkDateRange start end
    }


{-| A function that creates a DateRange by taking in two dates (start and end).

This function assumes that start <= end

-}
mkDateRange : Date -> Date -> DateRange
mkDateRange start end =
    { start = mkDate (year start) (month start) (day start)
    , end = mkDate (year end) (month end) (day end)
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
    , presetLastYear today
    , presetLastMonth today
    ]


{-| An opaque function for the default preset "Today"
-}
presetToday : Date -> Preset
presetToday today =
    { name = "Today"
    , dateRange = mkDateRange today today
    }


{-| An opaque function for the default preset "Yesterday"
-}
presetYesterday : Date -> Preset
presetYesterday today =
    let
        start =
            subDays 1 today

        end =
            subDays 1 today
    in
        { name = "Yesterday"
        , dateRange = mkDateRange start end
        }


{-| An opaque function for the default preset "Past Week"
-}
presetPastWeek : Date -> Preset
presetPastWeek today =
    let
        start =
            subDays 7 today

        end =
            today
    in
        { name = "Past Week"
        , dateRange = mkDateRange start end
        }


{-| An opaque function for the default preset "Past Month"
-}
presetPastMonth : Date -> Preset
presetPastMonth today =
    let
        start =
            addDays 1 <| subMonths 1 today

        end =
            today
    in
        { name = "Past Month"
        , dateRange = mkDateRange start end
        }


{-| An opaque function for the default preset "Past Year"
-}
presetPastYear : Date -> Preset
presetPastYear today =
    let
        start =
            addDays 1 <| subYears 1 today

        end =
            today
    in
        { name = "Past Year"
        , dateRange = mkDateRange start end
        }


{-| An opaque function for the default preset "Last Year"
-}
presetLastYear : Date -> Preset
presetLastYear today =
    let
        newYear =
            year <| subYears 1 today

        start =
            mkDate newYear Jan 1

        end =
            mkDate newYear Dec 31
    in
        { name = "Last Year"
        , dateRange = mkDateRange start end
        }


{-| An opaque function for the default preset "Last Month"
-}
presetLastMonth : Date -> Preset
presetLastMonth today =
    let
        newMonth =
            subMonths 1 today

        start =
            startOfMonth newMonth

        end =
            endOfMonth newMonth
    in
        { name = "Last Month"
        , dateRange = mkDateRange start end
        }


{-| A record of default settings for the daterangepicker.
-}
defaultSettings : Settings
defaultSettings =
    { placeholder = "Select a date..."
    , inputName = Nothing
    , inputId = Nothing
    , inputAttributes = []
    , presetOptions = defaultPresetOptions
    , restrictedDateRange = ToPresent
    , formatDateRange = formatDateRange
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
    , forceOpen = False
    , currentYear = prepareYear initDate
    , dateRange = Nothing
    , startDate = Nothing
    , endDate = Nothing
    , showPresets = False
    , presets = []
    , enabledDateRange = Nothing
    , settings = defaultSettings
    }


{-| The opaque initial command to get the current date, used within the
initial state.
-}
initCmd : Cmd Msg
initCmd =
    Task.perform InitCurrentDate Date.now


{-| The daterangepicker update function.
-}
update : Msg -> DateRangePicker -> ( DateRangePicker, Cmd Msg )
update msg (DateRangePicker ({ forceOpen, settings } as model)) =
    let
        ( newModel, cmds ) =
            case msg of
                InitCurrentDate date ->
                    let
                        presets =
                            mkPresets settings date

                        enabledDateRange =
                            mkEnabledDateRangeFromSettings settings date

                        newModel_ =
                            { model
                                | today = date
                                , currentYear = prepareYear date
                                , presets = presets
                                , enabledDateRange = enabledDateRange
                            }

                        newDateRange =
                            case model.dateRange of
                                Just a ->
                                    Just <| getNewDateRange newModel_ a

                                Nothing ->
                                    Nothing
                    in
                        { newModel_ | dateRange = newDateRange }
                            ! []

                PrevYear ->
                    let
                        prevYear =
                            prepareYear <|
                                mkDate (model.currentYear.year - 1) Jan 1
                    in
                        { model | currentYear = prevYear } ! []

                NextYear ->
                    let
                        nextYear =
                            prepareYear <|
                                mkDate (model.currentYear.year + 1) Jan 1
                    in
                        { model | currentYear = nextYear } ! []

                SetDateRange dateRange ->
                    let
                        newDateRange =
                            getNewDateRange model dateRange
                    in
                        { model
                            | dateRange = Just newDateRange
                            , startDate = Nothing
                            , endDate = Nothing
                            , showPresets = False
                            , currentYear = prepareYear newDateRange.end
                        }
                            ! []

                SetDate date ->
                    case ( model.startDate, model.endDate ) of
                        ( Just a, Just b ) ->
                            { model
                                | startDate = Just date
                                , endDate = Nothing
                                , dateRange = Nothing
                            }
                                ! []

                        ( Just a, Nothing ) ->
                            let
                                start =
                                    model.startDate

                                end =
                                    Just date

                                dateRange =
                                    case ( start, end ) of
                                        ( Just aa, Just bb ) ->
                                            if aa $<= bb then
                                                Just <|
                                                    mkDateRange
                                                        aa
                                                        bb
                                            else
                                                Nothing

                                        ( _, _ ) ->
                                            Nothing
                            in
                                case dateRange of
                                    Just a ->
                                        { model
                                            | endDate = Nothing
                                            , startDate = Nothing
                                            , dateRange = dateRange
                                        }
                                            ! []

                                    Nothing ->
                                        { model
                                            | startDate = Just date
                                            , endDate = Nothing
                                            , dateRange = Nothing
                                        }
                                            ! []

                        ( Nothing, Nothing ) ->
                            { model
                                | startDate = Just date
                                , dateRange = Nothing
                            }
                                ! []

                        ( _, _ ) ->
                            model ! []

                Focus ->
                    let
                        newYear =
                            case model.dateRange of
                                Just a ->
                                    prepareYear a.end

                                Nothing ->
                                    model.currentYear
                    in
                        { model
                            | open = True
                            , forceOpen = False
                            , currentYear = newYear
                        }
                            ! []

                Blur ->
                    { model | open = forceOpen } ! []

                MouseDown ->
                    { model | forceOpen = True } ! []

                MouseUp ->
                    { model | forceOpen = False } ! []

                Done ->
                    let
                        newModel =
                            { model | open = False, forceOpen = False }
                    in
                        case newModel.dateRange of
                            Just a ->
                                newModel ! []

                            Nothing ->
                                case newModel.startDate of
                                    Just b ->
                                        let
                                            newDateRange =
                                                mkDateRange b b
                                        in
                                            { newModel
                                                | dateRange = Just newDateRange
                                                , startDate = Nothing
                                                , endDate = Nothing
                                                , currentYear = prepareYear newDateRange.end
                                            }
                                                ! []

                                    Nothing ->
                                        newModel ! []

                Reset ->
                    initModel ! [ initCmd ]

                TogglePresets ->
                    { model | showPresets = not model.showPresets } ! []

                DoNothing ->
                    model ! []
    in
        (updateInputText newModel) !> [ cmds ]


{-| Expose if the daterange picker is open
-}
isOpen : DateRangePicker -> Bool
isOpen (DateRangePicker model) =
    model.open


{-| Expose the current selected daterange.
-}
getDateRange : DateRangePicker -> Maybe DateRange
getDateRange (DateRangePicker model) =
    model.dateRange


{-| Sets the current daterange for the daterangepicker.
-}
setDateRange : DateRange -> DateRangePicker -> DateRangePicker
setDateRange dateRange (DateRangePicker model) =
    DateRangePicker { model | dateRange = Just (getNewDateRange model dateRange) }


{-| Sets the settings for the daterange picker
-}
setSettings : Settings -> DateRangePicker -> DateRangePicker
setSettings settings (DateRangePicker model) =
    DateRangePicker { model | settings = settings }


{-| The daterange picker view. The date range passed is whatever date range it should treat as selected.
-}
view : DateRangePicker -> Html Msg
view (DateRangePicker ({ open, settings } as model)) =
    let
        potentialInputId =
            settings.inputId
                |> Maybe.map Attrs.id
                |> (List.singleton >> List.filterMap identity)

        dateInput =
            div
                ([ Attrs.name (settings.inputName ?> "")
                 , onBlur Blur
                 , onClick Focus
                 , onFocus Focus
                 , class "elm-daterangepicker--date-input"
                 ]
                    ++ settings.inputAttributes
                    ++ potentialInputId
                )
                [ model.inputText ?> settings.placeholder |> text ]
    in
        div [ class "elm-daterangepicker--container" ]
            [ dateInput
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
        onPicker ev =
            Json.succeed
                >> onWithOptions ev
                    { preventDefault = False
                    , stopPropagation = True
                    }

        content =
            case model.showPresets of
                False ->
                    getCalendar model

                True ->
                    getPresets model

        header =
            getHeader
    in
        div
            [ class "elm-daterangepicker--wrapper"
            , onPicker "mousedown" MouseDown
            , onPicker "mouseup" MouseUp
            ]
            [ header
            , content
            ]


{-| An opaque function that prints the daterangepicker calendar.
-}
getCalendar : Model -> Html Msg
getCalendar model =
    div [ class "elm-daterangepicker--calendar" ] <|
        List.concat
            [ getYearHeader model
            , getQuarters model
            ]


{-| An opaque function gets the Html Msg for the header of the daterange picker.
-}
getHeader : Html Msg
getHeader =
    div [ class "elm-daterangepicker--header" ]
        [ button [ onClick TogglePresets, class "elm-daterangepicker--presets-btn" ] [ i [ class "fa fa-cog" ] [], text "Presets" ]
        , button [ onClick Reset, class "elm-daterangepicker--reset-btn" ] [ i [ class "fa fa-ban" ] [], text "Reset" ]
        , button [ onClick Done, class "elm-daterangepicker--done-btn" ] [ i [ class "fa fa-check" ] [], text "Done" ]
        ]


{-| An opaque function that gets the Html Msg for the presets of the daterange picker.
-}
getPresets : Model -> Html Msg
getPresets model =
    div [ class "elm-daterangepicker--presets" ] <|
        List.map (getPreset model) model.presets


{-| An opaque function that gets the Html Msg for a given preset.
-}
getPreset : Model -> Preset -> Html Msg
getPreset model preset =
    let
        isDisabledPreset =
            isDisabledDate model preset.dateRange.start
                && isDisabledDate model preset.dateRange.end

        setDateRange =
            case isDisabledPreset of
                True ->
                    onClick DoNothing

                False ->
                    onClick <|
                        SetDateRange preset.dateRange

        className =
            String.join " " <|
                List.filter (\x -> x /= "")
                    [ "elm-daterangepicker--preset"
                    , mkClass "elm-daterangepicker--disabled" isDisabledPreset
                    ]
    in
        div [ class className, setDateRange ]
            [ span [ class "elm-daterangepicker--preset-name" ] [ text preset.name ]
            , span [ class "elm-daterangepicker--preset-range" ] [ text <| model.settings.formatDateRange preset.dateRange ]
            ]


{-| An opaque function that gets the year header Html Msg for the calendar.
-}
getYearHeader : Model -> List (Html Msg)
getYearHeader model =
    let
        start =
            mkDate model.currentYear.year Jan 1

        end =
            mkDate model.currentYear.year Dec 31

        isDisabledYear =
            isDisabledDate model start
                && isDisabledDate model end

        setYearRange =
            case isDisabledYear of
                True ->
                    onClick DoNothing

                False ->
                    onClick <|
                        SetDateRange <|
                            mkDateRange start end

        yrLabelClass =
            String.join " " <|
                List.filter (\x -> x /= "")
                    [ "elm-daterangepicker--yr-btn"
                    , "elm-daterangepicker--yr-label"
                    , mkClass "elm-daterangepicker--disabled" isDisabledYear
                    ]
    in
        [ div [ class "elm-daterangepicker--yr-label-wrapper" ]
            [ div [ class "elm-daterangepicker--yr-btn elm-daterangepicker--yr-prev", onClick PrevYear ] []
            , div [ class yrLabelClass, setYearRange ] [ text model.currentYear.name ]
            , div [ class "elm-daterangepicker--yr-btn elm-daterangepicker--yr-next", onClick NextYear ] []
            ]
        ]


{-| An opaque function that gets the Html Msg for the quarters of the calendar.
-}
getQuarters : Model -> List (Html Msg)
getQuarters model =
    let
        quarters =
            model.currentYear.quarters
    in
        List.map (getQuarter model) quarters


{-| An opaque function that gets the Html Msg for a given Quarter.
-}
getQuarter : Model -> Quarter -> Html Msg
getQuarter model qtr =
    let
        firstMonthOfQtr =
            List.head qtr.months

        lastMonthOfQtr =
            List.head <|
                List.reverse qtr.months
    in
        case ( firstMonthOfQtr, lastMonthOfQtr ) of
            ( Just firstMonth, Just lastMonth ) ->
                let
                    startOfQtr =
                        List.head firstMonth

                    endOfQtr =
                        List.head <|
                            List.reverse lastMonth

                    qtrDiv =
                        case ( startOfQtr, endOfQtr ) of
                            ( Just start, Just end ) ->
                                let
                                    isDisabledQtr =
                                        isDisabledDate model start
                                            && isDisabledDate model end

                                    setQtrDateRange =
                                        case isDisabledQtr of
                                            True ->
                                                onClick DoNothing

                                            False ->
                                                onClick <|
                                                    SetDateRange <|
                                                        mkDateRange start end

                                    className =
                                        String.join " " <|
                                            List.filter (\x -> x /= "")
                                                [ "elm-daterangepicker--qtr-label"
                                                , mkClass "elm-daterangepicker--disabled" isDisabledQtr
                                                ]

                                    qtrLabel =
                                        div [ class className, setQtrDateRange ] [ text qtr.name ]
                                in
                                    div [ class "elm-daterangepicker--qtr-row" ] <|
                                        List.concat
                                            [ [ qtrLabel ]
                                            , List.map (getMonth model) qtr.months
                                            ]

                            ( _, _ ) ->
                                text ""
                in
                    qtrDiv

            ( _, _ ) ->
                text ""


{-| An opaque function that gets the Html Msg for a given month of the calendar.
-}
getMonth : Model -> List Date -> Html Msg
getMonth model m =
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
                            , List.map (getDay model) m
                            ]

                    startOfMonth_ =
                        startOfMonth a

                    endOfMonth_ =
                        endOfMonth a

                    isDisabledMonth =
                        isDisabledDate model startOfMonth_
                            && isDisabledDate model endOfMonth_

                    setMonthDateRange =
                        case isDisabledMonth of
                            True ->
                                onClick DoNothing

                            False ->
                                onClick <|
                                    SetDateRange <|
                                        mkDateRange (startOfMonth a) (endOfMonth a)

                    className =
                        String.join " " <|
                            List.filter (\x -> x /= "")
                                [ "elm-daterangepicker--month-label"
                                , mkClass "elm-daterangepicker--disabled" isDisabledMonth
                                ]

                    monthDiv =
                        div [ class className, setMonthDateRange ]
                            [ text <|
                                formatMonth <|
                                    month a
                            ]
                in
                    div [ class "elm-daterangepicker--month" ] <|
                        List.concat
                            [ [ monthDiv ]
                            , getDaysOfWeek
                            , days
                            , padMonthRight (42 - List.length days)
                            ]

            _ ->
                text ""


{-| An opaque function that gets the Days of the Week Html Msg for the calendar.
-}
getDaysOfWeek : List (Html Msg)
getDaysOfWeek =
    let
        days =
            List.range 1 7

        go n =
            div [ class "elm-daterangepicker--dow" ]
                [ text <|
                    formatDay <|
                        dayFromInt n
                ]
    in
        List.map go days


{-| An opaque function taht pads the month from the left with filler days
in order to get the first of the month to line up correctly with the correct
day of the week.
-}
padMonthLeft : Date -> List (Html Msg)
padMonthLeft d =
    let
        dd =
            dayToInt <| dayOfWeek d

        n =
            dd - 1

        go =
            div [ class "elm-daterangepicker--day-filler" ] []
    in
        List.repeat n go


{-| An opaque function that pads the end of the month with filler days in order to fill
the month with 42 total days (days + filler days) to keep the size of each month in the
calendar the same size.
-}
padMonthRight : Int -> List (Html Msg)
padMonthRight n =
    let
        go =
            div [ class "elm-daterangepicker--day-filler" ] []
    in
        List.repeat n go


{-| An opaque function that gets the Html Msg for a Day.
-}
getDay : Model -> Date -> Html Msg
getDay model date =
    let
        isDisabledDate_ =
            isDisabledDate model date

        className =
            String.join " " <|
                List.filter (\x -> x /= "")
                    [ "elm-daterangepicker--day"
                    , mkClass "elm-daterangepicker--selected-range" <| isSelectedDateRange model date
                    , mkClass "elm-daterangepicker--disabled" isDisabledDate_
                    ]

        setDate =
            case isDisabledDate_ of
                True ->
                    onClick DoNothing

                False ->
                    onClick <| SetDate date
    in
        div [ class className, setDate ]
            [ text <|
                toString <|
                    day date
            ]


{-| An opaque function that returns a class name or an empty string
if the bool is true or not
-}
mkClass : String -> Bool -> String
mkClass cls bool =
    if bool then
        cls
    else
        ""


{-| An opaque function to check if the given date is a disabled date.
-}
isDisabledDate : Model -> Date -> Bool
isDisabledDate model date =
    case model.enabledDateRange of
        Nothing ->
            False

        Just dateRange ->
            case ( dateRange.start, dateRange.end ) of
                ( Just start, Just end ) ->
                    not <| inRange date <| mkDateRange start end

                ( Just start, Nothing ) ->
                    date $< start

                ( Nothing, Just end ) ->
                    date $> (addDays 1 end)

                ( Nothing, Nothing ) ->
                    False


{-| An opaque function the check if the given date is in the selected range.
-}
isSelectedDateRange : Model -> Date -> Bool
isSelectedDateRange model date =
    case model.dateRange of
        Just a ->
            inRange date a

        Nothing ->
            isStartOrEnd date model


{-| An opaque function that prepares the full year based on the given date.
-}
prepareYear : Date -> FullYear
prepareYear date =
    let
        yr =
            year date

        start =
            mkDate yr Jan 1

        end =
            mkDate yr Dec 31

        dates =
            datesInRange start end
    in
        { name = toString yr
        , year = yr
        , quarters =
            prepareQuarters <|
                LE.groupWhile (\x y -> (month x) == (month y)) dates
        }


{-| An opaque function that prepares the quarters of the full year
given the full list of dates for the year.
-}
prepareQuarters : List (List Date) -> List Quarter
prepareQuarters lst =
    let
        qs =
            chunksOfLeft 3 lst
    in
        List.indexedMap
            (\idx q ->
                { name =
                    String.concat
                        [ "Q"
                        , toString <| idx + 1
                        ]
                , months = q
                }
            )
            qs


{-| An opaque function to check if a given date is within a
given dateRange.
-}
inRange : Date -> DateRange -> Bool
inRange date { start, end } =
    let
        ( timeDate, timeStart, timeEnd ) =
            ( Date.toTime date
            , Date.toTime start
            , Date.toTime end
            )
    in
        if timeStart <= timeDate && timeEnd >= timeDate then
            True
        else
            False


{-| An opaque function that checks if the passed in date is equal
to the model's startDate or endDate
-}
isStartOrEnd : Date -> Model -> Bool
isStartOrEnd date model =
    case ( model.startDate, model.endDate ) of
        ( Just a, Just b ) ->
            Date.toTime a == Date.toTime date || Date.toTime b == Date.toTime date

        ( Just a, _ ) ->
            Date.toTime a == Date.toTime date

        ( _, Just b ) ->
            Date.toTime b == Date.toTime date

        ( _, _ ) ->
            False


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
                            if dateRange.start $>= start then
                                dateRange.start
                            else
                                start

                        newEnd =
                            if dateRange.end $>= start then
                                dateRange.end
                            else
                                start
                    in
                        mkDateRange newStart newEnd

                ( Nothing, Just end ) ->
                    let
                        newStart =
                            if dateRange.start $<= end then
                                dateRange.start
                            else
                                end

                        newEnd =
                            if dateRange.end $<= end then
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


{-| An opaque function that formats a daterange to a string.
-}
formatDateRange : DateRange -> String
formatDateRange dateRange =
    String.concat
        [ formatDate dateRange.start
        , " - "
        , formatDate dateRange.end
        ]


(?>) : Maybe a -> a -> a
(?>) =
    flip Maybe.withDefault


($<=) : Date -> Date -> Bool
($<=) a b =
    Date.toTime a <= Date.toTime b


($>=) : Date -> Date -> Bool
($>=) a b =
    Date.toTime a >= Date.toTime b


($<) : Date -> Date -> Bool
($<) a b =
    Date.toTime a < Date.toTime b


($>) : Date -> Date -> Bool
($>) a b =
    Date.toTime a >= Date.toTime b


(!) : Model -> List (Cmd Msg) -> ( Model, Cmd Msg )
(!) model cmds =
    ( model, Cmd.batch cmds )


(!>) : Model -> List (Cmd Msg) -> ( DateRangePicker, Cmd Msg )
(!>) model cmds =
    ( DateRangePicker model, Cmd.batch cmds )
