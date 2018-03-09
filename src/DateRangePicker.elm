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
        , DateRangePicker
        , mkPresetFromDateRange
        , mkPresetFromDates
        , defaultPresets
        , defaultSettings
        , defaultPresetOptions
        , init
        , update
        , isOpen
        , getDateRange
        , setDateRange
        , setSettings
        , setDateRangeFormat
        , setPlaceholder
        , setInputName
        , setInputId
        , setInputAttributes
        , setPresetOptions
        , setRestrictedDateRange
        , formatDateRange
        , view
        , getMinDate
        , getMaxDate
        )

{-| A customizable daterangepicker component.

@docs Msg, DateRangePicker
@docs init, update, isOpen, view, getDateRange, setDateRange


# Settings

@docs Settings, defaultSettings, setSettings, setDateRangeFormat, setPlaceholder, setInputName, setInputId, setInputAttributes, setPresetOptions, setRestrictedDateRange, formatDateRange, getMinDate, getMaxDate


## Presets

@docs PresetOptions, PresetOption, Preset, PresetSetting, PresetInterval, PresetRelativeToToday, defaultPresetOptions, defaultPresets, mkPresetFromDateRange, mkPresetFromDates

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
        , mkClassString
        , noPresets
        )


{-| A type representing messages that are passed within the DateRangePicker.
-}
type Msg
    = InitCurrentDate Date
    | PrevYear
    | NextYear
    | SetDateRange DateRange
    | SetDate Date
    | DoNothing
    | Click
    | MouseDown
    | MouseUp
    | Done
    | Reset
    | TogglePresets
    | HoverDay Date


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
    , hoveredDate : Maybe Date
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
    mkPresetFromDates "Today" today today


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
        mkPresetFromDates "Yesterday" start end


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
        mkPresetFromDates "Past Week" start end


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
        mkPresetFromDates "Past Month" start end


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
        mkPresetFromDates "Past Year" start end


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
        mkPresetFromDates "Last Year" start end


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
        mkPresetFromDates "Last Month" start end


{-| A record of default settings for the daterangepicker.
-}
defaultSettings : Settings
defaultSettings =
    { placeholder = "Select a date range..."
    , inputName = Nothing
    , inputId = Nothing
    , inputAttributes = []
    , presetOptions = defaultPresetOptions
    , restrictedDateRange = Off
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
    , hoveredDate = Nothing
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
                            mkEnabledDateRangeFromRestrictedDateRange settings.restrictedDateRange date

                        newModel_ =
                            { model
                                | today = mkDate (year date) (month date) (day date)
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
                            , open = False
                            , forceOpen = False
                            , hoveredDate = Nothing
                        }
                            $! []

                SetDate date ->
                    case ( model.startDate, model.endDate ) of
                        ( Just a, Just b ) ->
                            { model
                                | startDate = Just date
                                , endDate = Nothing
                                , dateRange = Nothing
                            }
                                $! []

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
                                            , open = False
                                            , forceOpen = False
                                            , hoveredDate = Nothing
                                        }
                                            $! []

                                    Nothing ->
                                        { model
                                            | startDate = Just date
                                            , endDate = Nothing
                                            , dateRange = Nothing
                                        }
                                            $! []

                        ( Nothing, Nothing ) ->
                            { model
                                | startDate = Just date
                                , dateRange = Nothing
                            }
                                $! []

                        ( _, _ ) ->
                            model $! []

                Click ->
                    let
                        newYear =
                            case model.dateRange of
                                Just a ->
                                    prepareYear a.end

                                Nothing ->
                                    model.currentYear

                        newOpen =
                            not model.open
                    in
                        { model
                            | open = newOpen
                            , forceOpen = False
                            , currentYear = newYear
                        }
                            $! []

                MouseDown ->
                    { model | forceOpen = True } $! []

                MouseUp ->
                    { model | forceOpen = False } $! []

                Done ->
                    let
                        newModel =
                            { model | open = False, forceOpen = False }
                    in
                        case newModel.dateRange of
                            Just a ->
                                newModel $! []

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
                                                , hoveredDate = Nothing
                                                , currentYear = prepareYear newDateRange.end
                                                , showPresets = False
                                            }
                                                $! []

                                    Nothing ->
                                        newModel $! []

                Reset ->
                    { model
                        | dateRange = Nothing
                        , startDate = Nothing
                        , endDate = Nothing
                        , hoveredDate = Nothing
                        , showPresets = False
                        , open = False
                        , forceOpen = False
                    }
                        $! [ initCmd ]

                TogglePresets ->
                    { model | showPresets = not model.showPresets } $! []

                HoverDay date ->
                    { model | hoveredDate = Just date } ! []

                DoNothing ->
                    model $! []
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


{-| Expose the min date in the enabled date range.
-}
getMinDate : DateRangePicker -> Maybe Date
getMinDate (DateRangePicker model) =
    case model.enabledDateRange of
        Just a ->
            case ( a.start, a.end ) of
                ( Just start, _ ) ->
                    Just start

                ( _, _ ) ->
                    Nothing

        Nothing ->
            Nothing


{-| Expose the max date in the enabled date range.
-}
getMaxDate : DateRangePicker -> Maybe Date
getMaxDate (DateRangePicker model) =
    case model.enabledDateRange of
        Just a ->
            case ( a.start, a.end ) of
                ( _, Just end ) ->
                    Just end

                ( _, _ ) ->
                    Nothing

        Nothing ->
            Nothing


{-| Sets the current daterange for the daterangepicker.
-}
setDateRange : Maybe DateRange -> DateRangePicker -> DateRangePicker
setDateRange dateRange (DateRangePicker model) =
    let
        newDateRange =
            case dateRange of
                Just a ->
                    Just <| getNewDateRange model a

                Nothing ->
                    Nothing
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
                 , Events.onClick Click
                 , Attrs.class "elm-fancy-daterangepicker--date-input"
                 ]
                    ++ settings.inputAttributes
                    ++ potentialInputId
                )
                [ i [ Attrs.class "fa fa-calendar" ] []
                , model.inputText ?> settings.placeholder |> text
                ]
    in
        div [ Attrs.class "elm-fancy-daterangepicker--container" ]
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
            [ Attrs.class "elm-fancy-daterangepicker--wrapper"
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
    div [ Attrs.class "elm-fancy-daterangepicker--calendar" ] <|
        List.concat
            [ getYearHeader model
            , getQuarters model
            ]


{-| An opaque function gets the Html Msg for the header of the daterange picker.
-}
getHeader : Html Msg
getHeader =
    div [ Attrs.class "elm-fancy-daterangepicker--header" ]
        [ div [ Events.onClick Done, Attrs.class "elm-fancy-daterangepicker--done-btn" ] [ i [ Attrs.class "fa fa-check" ] [], text "Done" ]
        , div [ Events.onClick TogglePresets, Attrs.class "elm-fancy-daterangepicker--presets-btn" ] [ i [ Attrs.class "fa fa-cog" ] [], text "Presets" ]
        , div [ Events.onClick Reset, Attrs.class "elm-fancy-daterangepicker--reset-btn" ] [ i [ Attrs.class "fa fa-ban" ] [], text "Reset" ]
        ]


{-| An opaque function that gets the Html Msg for the presets of the daterange picker.
-}
getPresets : Model -> Html Msg
getPresets model =
    div [ Attrs.class "elm-fancy-daterangepicker--presets" ] <|
        if List.length model.presets > 0 then
            List.map (getPreset model) model.presets
        else
            noPresets


{-| An opaque function that gets the Html Msg for a given preset.
-}
getPreset : Model -> Preset -> Html Msg
getPreset model preset =
    let
        isDisabledPreset =
            isDisabledDate model.enabledDateRange preset.dateRange.start
                && isDisabledDate model.enabledDateRange preset.dateRange.end

        setDateRange =
            case isDisabledPreset of
                True ->
                    Events.onClick DoNothing

                False ->
                    Events.onClick <|
                        SetDateRange preset.dateRange

        classString =
            mkClassString
                [ "elm-fancy-daterangepicker--preset"
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledPreset
                ]
    in
        div [ Attrs.class classString, setDateRange ]
            [ span [ Attrs.class "elm-fancy-daterangepicker--preset-name" ] [ text preset.name ]
            , span [ Attrs.class "elm-fancy-daterangepicker--preset-value" ] [ text <| model.settings.formatDateRange preset.dateRange ]
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

        yrLabelClassString =
            mkClassString
                [ "elm-fancy-daterangepicker--yr-btn"
                , "elm-fancy-daterangepicker--yr-label"
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledYear
                ]
    in
        [ div [ Attrs.class "elm-fancy-daterangepicker--yr-label-wrapper" ]
            [ div [ Attrs.class "elm-fancy-daterangepicker--yr-btn elm-fancy-daterangepicker--yr-prev", Events.onClick PrevYear ] []
            , div [ Attrs.class yrLabelClassString, setYearRange ] [ text model.currentYear.name ]
            , div [ Attrs.class "elm-fancy-daterangepicker--yr-btn elm-fancy-daterangepicker--yr-next", Events.onClick NextYear ] []
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

                                    classString =
                                        mkClassString
                                            [ "elm-fancy-daterangepicker--qtr-label"
                                            , mkClass "elm-fancy-daterangepicker--disabled" isDisabledQtr
                                            ]

                                    qtrLabel =
                                        div [ Attrs.class classString, setQtrDateRange ] [ text qtr.name ]
                                in
                                    div [ Attrs.class "elm-fancy-daterangepicker--qtr-row" ] <|
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

                    classString =
                        mkClassString
                            [ "elm-fancy-daterangepicker--month-label"
                            , mkClass "elm-fancy-daterangepicker--disabled" isDisabledMonth
                            ]

                    monthDiv =
                        div [ Attrs.class classString, setMonthDateRange ]
                            [ text <|
                                formatMonth <|
                                    month a
                            ]
                in
                    div [ Attrs.class "elm-fancy-daterangepicker--month" ] <|
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

        isSelectedDateRange_ =
            isSelectedDateRange model date

        isHoveredDateRange_ =
            isHoveredDateRange model date
                && not isDisabledDate_
                && not isSelectedDateRange_

        isToday_ =
            isToday model date

        classString =
            mkClassString
                [ "elm-fancy-daterangepicker--day"
                , mkClass "elm-fancy-daterangepicker--today" isToday_
                , mkClass "elm-fancy-daterangepicker--selected-range" isSelectedDateRange_
                , mkClass "elm-fancy-daterangepicker--hovered-range" isHoveredDateRange_
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledDate_
                ]

        setDate =
            case isDisabledDate_ of
                True ->
                    Events.onClick DoNothing

                False ->
                    Events.onClick <| SetDate date

        hoverDate =
            Events.onMouseOver <| HoverDay date
    in
        div [ Attrs.class classString, setDate, hoverDate ]
            [ text <|
                toString <|
                    day date
            ]


{-| An opaque function the check if the given date is in the selected range.
-}
isSelectedDateRange : Model -> Date -> Bool
isSelectedDateRange model date =
    case model.dateRange of
        Just a ->
            inRange date a

        Nothing ->
            isStartOrEnd model date


{-| An opaque function that checks if the given date is between the start date and hovered date.
-}
isHoveredDateRange : Model -> Date -> Bool
isHoveredDateRange model date =
    let
        hoveredDateRange =
            case ( model.startDate, model.hoveredDate ) of
                ( Just startDate, Just hoveredDate ) ->
                    if hoveredDate $>= startDate then
                        Just <| mkDateRange startDate hoveredDate
                    else
                        Nothing

                ( _, _ ) ->
                    Nothing
    in
        case hoveredDateRange of
            Just a ->
                inRange date a

            Nothing ->
                False


{-| An opaque function that checks if the passed in date is equal
to the model's startDate or endDate
-}
isStartOrEnd : Model -> Date -> Bool
isStartOrEnd model date =
    case ( model.startDate, model.endDate ) of
        ( Just a, Just b ) ->
            a $== date || b $== date

        ( Just a, _ ) ->
            a $== date

        ( _, Just b ) ->
            b $== date

        ( _, _ ) ->
            False


{-| An opaque function that checks if the passed in date is today.
-}
isToday : Model -> Date -> Bool
isToday model date =
    date $== model.today


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


{-| A function that formats a daterange to a string.
-}
formatDateRange : DateRange -> String
formatDateRange dateRange =
    String.concat
        [ formatDate dateRange.start
        , " - "
        , formatDate dateRange.end
        ]


(!>) : Model -> List (Cmd Msg) -> ( DateRangePicker, Cmd Msg )
(!>) model cmds =
    ( DateRangePicker model, Cmd.batch cmds )
