module DateRangePicker
    exposing
        ( Msg
        , DateRangePicker
        , Settings
        , PresetOption(..)
        , DateRange
        , PresetOptions
        , Preset
        , PresetSetting
        , defaultPresets
        , mkDateRange
        , mkPresetFromDateRange
        , mkPresetFromDates
        , init
        , update
        , defaultSettings
        , isOpen
        , view
        )

{-| A customizable daterangepicker component.

@docs Msg, DateRangePicker, DateRange
@docs init, update, isOpen, view, mkDateRange


# Settings

@docs Settings, defaultSettings


## Presets

@docs PresetOptions, PresetOption, Preset, PresetSetting, defaultPresets, mkPresetFromDateRange, mkPresetFromDates

-}

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, text, table, thead, th, tbody, tr, td, p, h1, input, button, a, i, span)
import Html.Attributes as Attrs exposing (class, colspan, type_, placeholder, value, href)
import Html.Events exposing (onClick, onDoubleClick, on, onBlur, onInput, onFocus, onWithOptions)
import Task
import List.Extra as LE
import DateRangePicker.Date exposing (initDate, mkDate, startOfMonth, endOfMonth, datesInRangeIncl, dayToInt, dayFromInt, formatDay, formatDate, formatMonth, daysInMonth, subDays, addDays, subMonths, addMonths, subYears, addYears)
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


{-| A type representing a date range with a start date and end date.
-}
type alias DateRange =
    { start : Date
    , end : Date
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
    }


{-| The model to be used within the DateRangePicker.
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
    }


{-| A type representing the disabled dates for the datepicker
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


{-| A type representing your preset options for your date range picker.
-}
type alias PresetOptions =
    { presetOption : PresetOption
    , presetSettings : List PresetSetting
    , presets : List Preset
    }


{-| A type representing which presets to use.
@DefaultPresets = Use the default presets from defaultPresets only.
@CustomPresetsFromSettings = Use only your custom built presets from a list of PresetSetting only.
@CustomPresets = Use only your custom built Presets.
@CustomOnly = Use only your custom build presets build from PresetSetting and your custom presets.
@AllPresets = Use all presets (default, customFromSettings, custom).
@NoPresets = Turn off Presets.
-}
type PresetOption
    = DefaultPresets
    | CustomPresetsFromSettings
    | CustomPresets
    | CustomOnly
    | AllPresets
    | NoPresets


{-| A type representing what the value in PresetSettings is measured in.


## Ex. value = 1 and PresetInterval = Days, this is interpretted as 1 Days.


## Ex. value = 4 and PresetI:nterval = Months, this is interpretted as 4 Months.

-}
type PresetInterval
    = Days
    | Months
    | Years


{-| A type representing how the preset is relative to today.


## If using ToToday, the preset daterange would use today as the end date, and the date from your PresetSettings as the end date.


## If using FromToday, the preset daterange would use today as the start date and the date from your PresetSettings as the end date.

-}
type PresetRelativeToToday
    = ToToday
    | FromToday


{-| A type used to generate preset dateranges.
@name = The name that you want to give the preset. i.e. "Past Month"
@interval = The interval in which you want to add/subtract the value from today.
@presetRelativeToToday = whether it is a range from [past - present] (ToToday) or [present - future] (FromToday)
@value = the number of your @interval that you are adding/subtracting.


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
@name = Name of the preset. i.e. "Past Month"
@dateRange = The daterange that is selected when selecting the preset.
-}
type alias Preset =
    { name : String
    , dateRange : DateRange
    }


{-| A type representing a restricted range for the datepicker. All dates not within the restricted date range will be disabled.
@Off = no restrictions, any date to any date can be chosen.
@ToPresent = from any date in the past up to today
@FromPresent = from today to any date in the future
@Past = from any date in the past up to yesterday
@Future = from tomorrow up to any date in the future
@Between date date = only between the two given dates
@To date = from any date in the past up to the given date
@From date = from the given date up to any date in the future
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
                customPresetsFromSettings_ ++ presetOptions.presets

            AllPresets ->
                defaultPresets_ ++ customPresetsFromSettings_

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


## This function assumes that start <= end

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
    , restrictedDateRange = Future
    }


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


{-| The inital model used within the inital state.
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
    }


{-| The initial command to get the current date, used within the
initial state.
-}
initCmd : Cmd Msg
initCmd =
    Task.perform InitCurrentDate Date.now


{-| The daterangepicker update function.
-}
update : Settings -> Msg -> DateRangePicker -> ( DateRangePicker, Cmd Msg )
update settings msg (DateRangePicker ({ forceOpen } as model)) =
    let
        ( newModel, cmds ) =
            case msg of
                InitCurrentDate date ->
                    let
                        presets =
                            mkPresets settings date

                        enabledDateRange =
                            mkEnabledDateRangeFromSettings settings date
                    in
                        { model
                            | today = date
                            , currentYear = prepareYear date
                            , presets = presets
                            , enabledDateRange = enabledDateRange
                        }
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
                    { model
                        | dateRange = Just dateRange
                        , startDate = Nothing
                        , endDate = Nothing
                        , showPresets = False
                        , currentYear = prepareYear dateRange.end
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


{-| The daterange picker view. The date range passed is whatever date range it should treat as selected.
-}
view : ( Maybe Date, Maybe Date ) -> Settings -> DateRangePicker -> Html Msg
view ( selectedStartDate, selectedEndDate ) settings (DateRangePicker ({ open } as model)) =
    let
        potentialInputId =
            settings.inputId
                |> Maybe.map Attrs.id
                |> (List.singleton >> List.filterMap identity)

        inputCommon xs =
            input
                ([ Attrs.name (settings.inputName ?> "")
                 , type_ "text"
                 , onBlur Blur
                 , onClick Focus
                 , onFocus Focus
                 ]
                    ++ settings.inputAttributes
                    ++ potentialInputId
                    ++ xs
                )
                []

        dateInput =
            inputCommon
                [ placeholder settings.placeholder
                , class "elm-daterangepicker--date-input"
                , model.inputText
                    |> Maybe.withDefault
                        ""
                    |> value
                ]
    in
        div [ class "elm-daterangepicker--container" ]
            [ dateInput
            , if open then
                dateRangePicker model
              else
                text ""
            ]


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
                    printCalendar model

                True ->
                    printPresets model

        header =
            printHeader
    in
        div
            [ class "elm-daterangepicker--wrapper"
            , onPicker "mousedown" MouseDown
            , onPicker "mouseup" MouseUp
            ]
            [ header
            , content
            ]


printCalendar : Model -> Html Msg
printCalendar model =
    div [ class "elm-daterangepicker--calendar" ] <|
        printYearLabel model.currentYear
            ++ printQuarters model


printHeader : Html Msg
printHeader =
    div [ class "elm-daterangepicker--header" ]
        [ button [ onClick TogglePresets, class "elm-daterangepicker--presets-btn" ] [ i [ class "fa fa-cog" ] [], text "Presets" ]
        , button [ onClick Reset, class "elm-daterangepicker--reset-btn" ] [ i [ class "fa fa-ban" ] [], text "Reset" ]
        , button [ onClick Done, class "elm-daterangepicker--done-btn" ] [ i [ class "fa fa-check" ] [], text "Done" ]
        ]


printPresets : Model -> Html Msg
printPresets model =
    div [ class "elm-daterangepicker--presets" ] <|
        List.map printPreset model.presets


printPreset : Preset -> Html Msg
printPreset preset =
    let
        setDateRange =
            onClick <|
                SetDateRange preset.dateRange
    in
        div [ class "elm-daterangepicker--preset", setDateRange ]
            [ span [ class "elm-daterangepicker--preset-name" ] [ text preset.name ]
            , span [ class "elm-daterangepicker--preset-range" ] [ text <| formatDateRange preset.dateRange ]
            ]


printYearLabel : FullYear -> List (Html Msg)
printYearLabel fullYear =
    let
        start =
            mkDate fullYear.year Jan 1

        end =
            mkDate fullYear.year Dec 31

        setYearRange =
            onClick <|
                SetDateRange <|
                    mkDateRange start end
    in
        [ div [ class "elm-daterangepicker--yr-label-wrapper" ]
            [ div [ class "elm-daterangepicker--yr-btn elm-daterangepicker--yr-prev", onClick PrevYear ] []
            , div [ class "elm-daterangepicker--yr-btn elm-daterangepicker--yr-label", setYearRange ] [ text fullYear.name ]
            , div [ class "elm-daterangepicker--yr-btn elm-daterangepicker--yr-next", onClick NextYear ] []
            ]
        ]


printQuarters : Model -> List (Html Msg)
printQuarters model =
    let
        quarters =
            model.currentYear.quarters
    in
        List.map (printQuarter model) quarters


printQuarter : Model -> Quarter -> Html Msg
printQuarter model qtr =
    let
        m1 =
            List.head qtr.months

        m2 =
            List.head <|
                List.reverse qtr.months
    in
        case ( m1, m2 ) of
            ( Just a, Just b ) ->
                let
                    startOfQuarter =
                        List.head a

                    endOfQuarter =
                        List.head <|
                            List.reverse b

                    setQuarterDateRange =
                        case ( startOfQuarter, endOfQuarter ) of
                            ( Just aa, Just bb ) ->
                                onClick <|
                                    SetDateRange <|
                                        mkDateRange aa bb

                            ( _, _ ) ->
                                onClick DoNothing

                    monthDiv =
                        div [ class "elm-daterangepicker--qtr-row" ] <|
                            ([ div [ class "elm-daterangepicker--qtr-label", setQuarterDateRange ] [ text qtr.name ] ]
                                ++ List.map (printMonth model) qtr.months
                            )
                in
                    monthDiv

            ( _, _ ) ->
                text ""


printMonth : Model -> List Date -> Html Msg
printMonth model m =
    let
        h =
            List.head m
    in
        case h of
            Just a ->
                let
                    days =
                        [] ++ padDaysLeft a ++ List.map (printDay model) m

                    setMonthDateRange =
                        onClick <|
                            SetDateRange <|
                                mkDateRange (startOfMonth a) (endOfMonth a)

                    monthDiv =
                        div [ class "elm-daterangepicker--month-label", setMonthDateRange ]
                            [ text <|
                                formatMonth <|
                                    month a
                            ]
                in
                    div [ class "elm-daterangepicker--month" ]
                        ([ monthDiv ]
                            ++ printDaysOfWeek
                            ++ days
                            ++ padMonth (42 - List.length days)
                        )

            _ ->
                text ""


printDaysOfWeek : List (Html Msg)
printDaysOfWeek =
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


padDaysLeft : Date -> List (Html Msg)
padDaysLeft d =
    let
        dd =
            dayToInt <| dayOfWeek d

        n =
            dd - 1

        go =
            div [ class "elm-daterangepicker--day-filler" ] []
    in
        List.repeat n go


padMonth : Int -> List (Html Msg)
padMonth n =
    let
        go =
            div [ class "elm-daterangepicker--day-filler" ] []
    in
        List.repeat n go


printDay : Model -> Date -> Html Msg
printDay model date =
    let
        isDisabledDate_ =
            isDisabledDate model date

        className =
            String.join " " <|
                List.filter (\x -> x /= "")
                    [ "elm-daterangepicker--day"
                    , mkClass "elm-daterangepicker--selected-range" <| isSelectedDateRange model date
                    , mkClass "elm-daterangepicker--disabled-date" isDisabledDate_
                    ]

        setDate =
            onClick <| SetDate date
    in
        div [ class className, setDate ]
            [ text <|
                toString <|
                    day date
            ]


mkClass : String -> Bool -> String
mkClass cls bool =
    if bool then
        cls
    else
        ""


isDisabledDate : Model -> Date -> Bool
isDisabledDate model date =
    case model.enabledDateRange of
        Nothing ->
            False

        Just dateRange ->
            case ( dateRange.start, dateRange.end ) of
                ( Just start, Just end ) ->
                    inRange date <| mkDateRange start end

                ( Just start, Nothing ) ->
                    start $> date

                ( Nothing, Just end ) ->
                    end $< date

                ( Nothing, Nothing ) ->
                    False


isSelectedDateRange : Model -> Date -> Bool
isSelectedDateRange model date =
    case model.dateRange of
        Just a ->
            inRange date a

        Nothing ->
            isStartOrEnd date model


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
            datesInRangeIncl start end
    in
        { name = toString yr
        , year = yr
        , quarters =
            prepareQuarters <|
                LE.groupWhile (\x y -> (month x) == (month y)) dates
        }


prepareQuarters : List (List Date) -> List Quarter
prepareQuarters lst =
    let
        qs =
            chunksOfLeft 3 lst
    in
        List.indexedMap (\idx q -> { name = "Q" ++ (toString (idx + 1)), months = q }) qs


{-| A function to check if a given date is within a
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


{-| A function that checks if the passed in date is equal
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


{-| A function that updates the inputText based on the
model's selected dateRange
-}
updateInputText : Model -> Model
updateInputText model =
    case model.dateRange of
        Just a ->
            { model | inputText = Just <| formatDateRange a }

        Nothing ->
            { model | inputText = Nothing }


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


formatDateRange : DateRange -> String
formatDateRange dateRange =
    String.concat
        [ formatDate dateRange.start
        , " - "
        , formatDate dateRange.end
        ]
