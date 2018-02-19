module DatePicker
    exposing
        ( Msg
        , Settings
        , PresetOptions
        , PresetOption(..)
        , PresetInterval(..)
        , PresetRelativeToToday(..)
        , PresetSetting
        , Preset
        , DatePicker
        , mkPresetFromDate
        , defaultPresets
        , defaultSettings
        , defaultPresetOptions
        , init
        , update
        , isOpen
        , getDate
        , setDate
        , setSettings
        , setDateFormat
        , view
        )

{-| A customizable daterangepicker component.

@docs Msg, DatePicker
@docs init, update, isOpen, view, getDate, setDate


# Settings

@docs Settings, defaultSettings, setSettings, setDateFormat


## Presets

@docs PresetOptions, PresetOption, Preset, PresetSetting, PresetInterval, PresetRelativeToToday, defaultPresetOptions, defaultPresets, mkPresetFromDate

-}

import Html.Attributes as Attrs
import Html.Events as Events
import Task
import Date
    exposing
        ( Date
        , Day(..)
        , Month(..)
        , day
        , dayOfWeek
        , month
        , year
        )
import Html
    exposing
        ( Html
        , div
        , text
        , input
        , button
        , i
        , span
        )
import DateRangePicker.Date
    exposing
        ( initDate
        , mkDate
        , startOfMonth
        , endOfMonth
        , datesInRange
        , dayToInt
        , dayFromInt
        , formatDay
        , formatDate
        , formatMonth
        , daysInMonth
        , subDays
        , addDays
        , subMonths
        , addMonths
        , subYears
        , addYears
        , ($==)
        , ($<=)
        , ($>=)
        , ($<)
        , ($>)
        )
import DateRangePicker.Common
    exposing
        ( RestrictedDateRange(..)
        , DateRange
        , mkDateRange
        )
import DateRangePicker.Common.Internal
    exposing
        ( FullYear
        , Quarter
        , EnabledDateRange
        , (?>)
        , ($!)
        , chunksOfLeft
        , inRange
        , prepareQuarters
        , prepareYear
        , padMonthLeft
        , padMonthRight
        , onPicker
        , mkEnabledDateRangeFromRestrictedDateRange
        , mkClass
        , isDisabledDate
        , getDaysOfWeek
        )


{-| A type representing messages that are passed within the DatePicker.
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


{-| The opaque model to be used within the DatePicker.
-}
type alias Model =
    { today : Date
    , inputText : Maybe String
    , open : Bool
    , forceOpen : Bool
    , currentYear : FullYear
    , date : Maybe Date
    , showPresets : Bool
    , presets : List Preset
    , enabledDateRange : Maybe EnabledDateRange
    , settings : Settings
    }


{-| The settings that the DatePicker uses.
-}
type alias Settings =
    { placeholder : String
    , inputName : Maybe String
    , inputId : Maybe String
    , inputAttributes : List (Html.Attribute Msg)
    , presetOptions : PresetOptions
    , restrictedDateRange : RestrictedDateRange
    , formatDate : Date -> String
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

  - BeforeToday will subtract any value from today and use that date.
  - AfterToday will add any value form today and use that date.
  - Today will use today.
  - Tomorrow will use tomorrow.
  - Yesterday will use yesterday.

-}
type PresetRelativeToToday
    = BeforeToday
    | AfterToday
    | Today
    | Tomorrow
    | Yesterday


{-| A type used to generate preset dateranges.

  - *name* = The name that you want to give the preset. i.e. "Past Month"
  - *interval* = The interval in which you want to add/subtract the value from today.
  - *presetRelativeToToday* = whether it is a range from [past - present] (ToToday) or [present - future] (FromToday)
  - *value* = the number of your @interval that you are adding/subtracting.


## Example

    { name = "1 Month Ago"
    , interval = Months
    , presetRelativeToday = BeforeToday
    , value = 1
    }

-}
type alias PresetSetting =
    { name : String
    , interval : PresetInterval
    , presetRelativeToToday : PresetRelativeToToday
    , value : Int
    }


{-| A type that represents a preset date.

  - *name* = Name of the preset. i.e. "Past Month"
  - *date* = The date that is selected when selecting the preset.

-}
type alias Preset =
    { name : String
    , date : Date
    }


{-| The DatePicker model.
-}
type DatePicker
    = DatePicker Model


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
        date =
            case presetRelativeToToday of
                BeforeToday ->
                    case interval of
                        Days ->
                            subDays value today

                        Months ->
                            subMonths value today

                        Years ->
                            subYears value today

                AfterToday ->
                    case interval of
                        Days ->
                            addDays value today

                        Months ->
                            addMonths value today

                        Years ->
                            addYears value today

                Today ->
                    today

                Tomorrow ->
                    addDays 1 today

                Yesterday ->
                    subDays 1 today
    in
        mkPresetFromDate name date


{-| A function that creates a Preset from a name and a date
-}
mkPresetFromDate : String -> Date -> Preset
mkPresetFromDate name date =
    { name = name
    , date = mkDate (year date) (month date) (day date)
    }


{-| An opaque function used to make the default presets
-}
defaultPresets : Date -> List Preset
defaultPresets today =
    [ presetTomorrow today
    , presetToday today
    , presetYesterday today
    ]


{-| An opaque function for the default preset "Today"
-}
presetToday : Date -> Preset
presetToday today =
    { name = "Today"
    , date = mkDate (year today) (month today) (day today)
    }


{-| An opaque function for the default preset "Yesterday"
-}
presetYesterday : Date -> Preset
presetYesterday today =
    let
        yesterday =
            subDays 1 today
    in
        { name = "Yesterday"
        , date = mkDate (year yesterday) (month yesterday) (day yesterday)
        }


{-| An opaque function for the default preset "Tomorrow"
-}
presetTomorrow : Date -> Preset
presetTomorrow today =
    let
        tomorrow =
            addDays 1 today
    in
        { name = "Tomorrow"
        , date = mkDate (year tomorrow) (month tomorrow) (day tomorrow)
        }


{-| A record of default settings for the datepicker.
-}
defaultSettings : Settings
defaultSettings =
    { placeholder = "Select a date..."
    , inputName = Nothing
    , inputId = Nothing
    , inputAttributes = []
    , presetOptions = defaultPresetOptions
    , restrictedDateRange = Off
    , formatDate = formatDate
    }


{-| A record of default preset options for the datepicker.
-}
defaultPresetOptions : PresetOptions
defaultPresetOptions =
    { presetOption = DefaultPresets
    , presetSettings = []
    , presets = []
    }


{-| The default initial state of the DatePicker. You must execute
the returned command in order to set the current date and for the
datepicker to behave correctly.
-}
init : ( DatePicker, Cmd Msg )
init =
    ( DatePicker initModel
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
    , date = Nothing
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


{-| The datepicker update function.
-}
update : Msg -> DatePicker -> ( DatePicker, Cmd Msg )
update msg (DatePicker ({ forceOpen, settings } as model)) =
    let
        ( newModel, cmds ) =
            case msg of
                InitCurrentDate date ->
                    let
                        presets =
                            mkPresets settings date

                        enabledDateRange =
                            mkEnabledDateRangeFromRestrictedDateRange settings.restrictedDateRange date

                        newModel_ =
                            { model
                                | today = date
                                , currentYear = prepareYear date
                                , presets = presets
                                , enabledDateRange = enabledDateRange
                            }

                        newDate =
                            case model.date of
                                Just a ->
                                    Just <| getNewDate newModel_ a

                                Nothing ->
                                    Nothing
                    in
                        { newModel_ | date = newDate }
                            $! []

                PrevYear ->
                    let
                        prevYear =
                            prepareYear <|
                                mkDate (model.currentYear.year - 1) Jan 1
                    in
                        { model | currentYear = prevYear } $! []

                NextYear ->
                    let
                        nextYear =
                            prepareYear <|
                                mkDate (model.currentYear.year + 1) Jan 1
                    in
                        { model | currentYear = nextYear } $! []

                SetDate date ->
                    let
                        newDate =
                            getNewDate model date
                    in
                        { model
                            | date = Just newDate
                            , showPresets = False
                            , currentYear = prepareYear date
                            , open = False
                            , forceOpen = False
                        }
                            $! []

                Focus ->
                    let
                        newYear =
                            case model.date of
                                Just a ->
                                    prepareYear a

                                Nothing ->
                                    model.currentYear
                    in
                        { model
                            | open = True
                            , forceOpen = False
                            , currentYear = newYear
                        }
                            $! []

                Blur ->
                    { model | open = forceOpen } $! []

                MouseDown ->
                    { model | forceOpen = True } $! []

                MouseUp ->
                    { model | forceOpen = False } $! []

                Done ->
                    let
                        newModel =
                            { model
                                | open = False
                                , forceOpen = False
                                , showPresets = False
                            }
                    in
                        case newModel.date of
                            Just a ->
                                { newModel | currentYear = prepareYear a } $! []

                            Nothing ->
                                newModel $! []

                Reset ->
                    initModel $! [ initCmd ]

                TogglePresets ->
                    { model | showPresets = not model.showPresets } $! []

                DoNothing ->
                    model $! []

                _ ->
                    model $! []
    in
        (updateInputText newModel) !> [ cmds ]


{-| Expose if the daterange picker is open
-}
isOpen : DatePicker -> Bool
isOpen (DatePicker model) =
    model.open


{-| Expose the current selected daterange.
-}
getDate : DatePicker -> Maybe Date
getDate (DatePicker model) =
    model.date


{-| Sets the current date for the datepicker.
-}
setDate : Date -> DatePicker -> DatePicker
setDate date (DatePicker model) =
    DatePicker { model | date = Just (getNewDate model date) }


{-| Sets the date formatter for the datepicker.
-}
setDateFormat : (Date -> String) -> DatePicker -> DatePicker
setDateFormat dateFormat (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | formatDate = dateFormat }
    in
        DatePicker { model | settings = newSettings }


{-| Sets the settings for the daterange picker
-}
setSettings : Settings -> DatePicker -> DatePicker
setSettings settings (DatePicker model) =
    DatePicker { model | settings = settings }


{-| The daterange picker view. The date range passed is whatever date range it should treat as selected.
-}
view : DatePicker -> Html Msg
view (DatePicker ({ open, settings } as model)) =
    let
        potentialInputId =
            settings.inputId
                |> Maybe.map Attrs.id
                |> (List.singleton >> List.filterMap identity)

        dateInput =
            div
                ([ Attrs.name (settings.inputName ?> "")
                 , Events.onBlur Blur
                 , Events.onClick Focus
                 , Events.onFocus Focus
                 , Attrs.class "elm-daterangepicker--date-input"
                 ]
                    ++ settings.inputAttributes
                    ++ potentialInputId
                )
                [ i [ Attrs.class "fa fa-calendar" ] []
                , model.inputText ?> settings.placeholder |> text
                ]
    in
        div [ Attrs.class "elm-daterangepicker--container" ]
            [ dateInput
            , if open then
                datePicker model
              else
                text ""
            ]


{-| An opaque function to create the daterange picker view.
-}
datePicker : Model -> Html Msg
datePicker model =
    let
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
            [ Attrs.class "elm-daterangepicker--wrapper"
            , onPicker "mousedown" MouseDown
            , onPicker "mouseup" MouseUp
            ]
            [ header
            , content
            ]


{-| An opaque function that prints the datepicker calendar.
-}
getCalendar : Model -> Html Msg
getCalendar model =
    div [ Attrs.class "elm-daterangepicker--calendar" ] <|
        List.concat
            [ getYearHeader model
            , getQuarters model
            ]


{-| An opaque function gets the Html Msg for the header of the daterange picker.
-}
getHeader : Html Msg
getHeader =
    div [ Attrs.class "elm-daterangepicker--header" ]
        [ button [ Events.onClick Done, Attrs.class "elm-daterangepicker--done-btn" ] [ i [ Attrs.class "fa fa-check" ] [], text "Done" ]
        , button [ Events.onClick TogglePresets, Attrs.class "elm-daterangepicker--presets-btn" ] [ i [ Attrs.class "fa fa-cog" ] [], text "Presets" ]
        , button [ Events.onClick Reset, Attrs.class "elm-daterangepicker--reset-btn" ] [ i [ Attrs.class "fa fa-ban" ] [], text "Reset" ]
        ]


{-| An opaque function that gets the Html Msg for the presets of the daterange picker.
-}
getPresets : Model -> Html Msg
getPresets model =
    div [ Attrs.class "elm-daterangepicker--presets" ] <|
        List.map (getPreset model) model.presets


{-| An opaque function that gets the Html Msg for a given preset.
-}
getPreset : Model -> Preset -> Html Msg
getPreset model preset =
    let
        isDisabledPreset =
            isDisabledDate model.enabledDateRange preset.date

        setDate =
            case isDisabledPreset of
                True ->
                    Events.onClick DoNothing

                False ->
                    Events.onClick <|
                        SetDate preset.date

        className =
            String.join " " <|
                List.filter (\x -> x /= "")
                    [ "elm-daterangepicker--preset"
                    , mkClass "elm-daterangepicker--disabled" isDisabledPreset
                    ]
    in
        div [ Attrs.class className, setDate ]
            [ span [ Attrs.class "elm-daterangepicker--preset-name" ] [ text preset.name ]
            , span [ Attrs.class "elm-daterangepicker--preset-range" ] [ text <| model.settings.formatDate preset.date ]
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
            isDisabledDate model.enabledDateRange start
                && isDisabledDate model.enabledDateRange end

        setYearRange =
            case isDisabledYear of
                True ->
                    Events.onClick DoNothing

                False ->
                    Events.onClick <|
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
        [ div [ Attrs.class "elm-daterangepicker--yr-label-wrapper" ]
            [ div [ Attrs.class "elm-daterangepicker--yr-btn elm-daterangepicker--yr-prev", Events.onClick PrevYear ] []
            , div [ Attrs.class yrLabelClass, setYearRange ] [ text model.currentYear.name ]
            , div [ Attrs.class "elm-daterangepicker--yr-btn elm-daterangepicker--yr-next", Events.onClick NextYear ] []
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
                                        isDisabledDate model.enabledDateRange start
                                            && isDisabledDate model.enabledDateRange end

                                    setQtrDateRange =
                                        case isDisabledQtr of
                                            True ->
                                                Events.onClick DoNothing

                                            False ->
                                                Events.onClick <|
                                                    SetDateRange <|
                                                        mkDateRange start end

                                    className =
                                        String.join " " <|
                                            List.filter (\x -> x /= "")
                                                [ "elm-daterangepicker--qtr-label"
                                                , mkClass "elm-daterangepicker--disabled" isDisabledQtr
                                                ]

                                    qtrLabel =
                                        div [ Attrs.class className, setQtrDateRange ] [ text qtr.name ]
                                in
                                    div [ Attrs.class "elm-daterangepicker--qtr-row" ] <|
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
                        isDisabledDate model.enabledDateRange startOfMonth_
                            && isDisabledDate model.enabledDateRange endOfMonth_

                    setMonthDateRange =
                        case isDisabledMonth of
                            True ->
                                Events.onClick DoNothing

                            False ->
                                Events.onClick <|
                                    SetDateRange <|
                                        mkDateRange (startOfMonth a) (endOfMonth a)

                    className =
                        String.join " " <|
                            List.filter (\x -> x /= "")
                                [ "elm-daterangepicker--month-label"
                                , mkClass "elm-daterangepicker--disabled" isDisabledMonth
                                ]

                    monthDiv =
                        div [ Attrs.class className, setMonthDateRange ]
                            [ text <|
                                formatMonth <|
                                    month a
                            ]
                in
                    div [ Attrs.class "elm-daterangepicker--month" ] <|
                        List.concat
                            [ [ monthDiv ]
                            , getDaysOfWeek
                            , days
                            , padMonthRight (42 - List.length days)
                            ]

            _ ->
                text ""


{-| An opaque function that gets the Html Msg for a Day.
-}
getDay : Model -> Date -> Html Msg
getDay model date =
    let
        isDisabledDate_ =
            isDisabledDate model.enabledDateRange date

        className =
            String.join " " <|
                List.filter (\x -> x /= "")
                    [ "elm-daterangepicker--day"
                    , mkClass "elm-daterangepicker--selected-range" <| isSelectedDate model date
                    , mkClass "elm-daterangepicker--disabled" isDisabledDate_
                    ]

        setDate =
            case isDisabledDate_ of
                True ->
                    Events.onClick DoNothing

                False ->
                    Events.onClick <| SetDate date
    in
        div [ Attrs.class className, setDate ]
            [ text <|
                toString <|
                    day date
            ]


{-| An opaque function the check if the given date is in the selected range.
-}
isSelectedDate : Model -> Date -> Bool
isSelectedDate model date =
    case model.date of
        Just a ->
            a $== date

        Nothing ->
            False


{-| An opaque function that gets the new date range from a selected date range
and the enabled dates of the date picker.
-}
getNewDate : Model -> Date -> Date
getNewDate model date =
    case model.enabledDateRange of
        Just dr ->
            case ( dr.start, dr.end ) of
                ( Just start, Just end ) ->
                    let
                        newDate =
                            if inRange date <| mkDateRange start end then
                                date
                            else
                                end
                    in
                        newDate

                ( Just start, Nothing ) ->
                    let
                        newDate =
                            if date $>= start then
                                date
                            else
                                start
                    in
                        newDate

                ( Nothing, Just end ) ->
                    let
                        newDate =
                            if date $<= end then
                                date
                            else
                                end
                    in
                        newDate

                ( Nothing, Nothing ) ->
                    date

        Nothing ->
            date


{-| An opaque function that updates the inputText based on the
model's selected dateRange
-}
updateInputText : Model -> Model
updateInputText model =
    case model.date of
        Just a ->
            { model | inputText = Just <| formatDate a }

        Nothing ->
            { model | inputText = Nothing }


{-| An opaque function that formats a daterange to a string.
-}
formatDateRange : DateRange -> String
formatDateRange dateRange =
    String.concat
        [ formatDate dateRange.start
        , " - "
        , formatDate dateRange.end
        ]


(!>) : Model -> List (Cmd Msg) -> ( DatePicker, Cmd Msg )
(!>) model cmds =
    ( DatePicker model, Cmd.batch cmds )
