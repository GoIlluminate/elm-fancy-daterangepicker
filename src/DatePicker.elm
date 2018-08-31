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
        , setOpen
        , getPresets
        , getDate
        , setDate
        , setSettings
        , setDateFormat
        , setPlaceholder
        , setInputName
        , setInputId
        , setInputIcon
        , setInputAttributes
        , setPresetOptions
        , setRestrictedDateRange
        , view
        )

{-| A customizable daterangepicker component.

@docs Msg, DatePicker
@docs init, update, isOpen, setOpen, view, getDate, setDate


# Settings

@docs Settings, defaultSettings, setSettings, setDateFormat, setPlaceholder, setInputName, setInputId, setInputIcon, setInputAttributes, setPresetOptions, setRestrictedDateRange


## Presets

@docs PresetOptions, PresetOption, Preset, PresetSetting, PresetInterval, PresetRelativeToToday, defaultPresetOptions, defaultPresets, mkPresetFromDate, getPresets

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
        , month
        , year
        )
import Html
    exposing
        ( Html
        , div
        , text
        , i
        , span
        )
import DateRangePicker.Date
    exposing
        ( initDate
        , mkDate
        , startOfMonth
        , endOfMonth
        , formatDate
        , formatMonth
        , subDays
        , addDays
        , subMonths
        , addMonths
        , subYears
        , addYears
        , dateEqualTo
        , dateGreaterThanOrEqualTo
        , dateLessThanOrEqualTo
        )
import DateRangePicker.Common
    exposing
        ( RestrictedDateRange(..)
        , mkDateRange
        , inRange
        )
import DateRangePicker.Common.Internal
    exposing
        ( FullYear
        , Quarter
        , EnabledDateRange
        , prepareYear
        , padMonthLeft
        , padMonthRight
        , onPicker
        , mkEnabledDateRangeFromRestrictedDateRange
        , mkClass
        , isDisabledDate
        , renderDaysOfWeek
        , mkClassString
        , noPresets
        )


{-| A type representing messages that are passed within the DatePicker.
-}
type Msg
    = InitCurrentDate Date
    | PrevYear
    | NextYear
    | SetDate Date
    | DoNothing
    | Click
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
    , inputIcon : Maybe (Html Msg)
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


{-| A function to make presets from settings and a date
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
        mkPresetFromDate "Yesterday" yesterday


{-| An opaque function for the default preset "Tomorrow"
-}
presetTomorrow : Date -> Preset
presetTomorrow today =
    let
        tomorrow =
            addDays 1 today
    in
        mkPresetFromDate "Yesterday" tomorrow


{-| A record of default settings for the datepicker.
-}
defaultSettings : Settings
defaultSettings =
    { placeholder = "Select a date..."
    , inputName = Nothing
    , inputId = Nothing
    , inputIcon = Nothing
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
update msg (DatePicker ({ settings } as model)) =
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
                                | today = mkDate (year date) (month date) (day date)
                                , currentYear = prepareYear date
                                , presets = presets
                                , enabledDateRange = enabledDateRange
                            }

                        newDate =
                            case model.date of
                                Just a ->
                                    Just <| getNewDate newModel a

                                Nothing ->
                                    Nothing
                    in
                        { newModel | date = newDate } ! []

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
                            ! []

                Click ->
                    let
                        newYear =
                            case model.date of
                                Just a ->
                                    prepareYear a

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
                            ! []

                MouseDown ->
                    { model | forceOpen = True } ! []

                MouseUp ->
                    { model | forceOpen = False } ! []

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
                                { newModel | currentYear = prepareYear a } ! []

                            Nothing ->
                                newModel ! []

                Reset ->
                    { model
                        | date = Nothing
                        , showPresets = False
                        , open = False
                        , forceOpen = False
                    }
                        ! [ initCmd ]

                TogglePresets ->
                    { model | showPresets = not model.showPresets } ! []

                DoNothing ->
                    model ! []
    in
        updateInputText updatedModel !> [ cmds ]


{-| Expose if the daterange picker is open
-}
isOpen : DatePicker -> Bool
isOpen (DatePicker model) =
    model.open


{-| Sets the the open state of the DatePicker
-}
setOpen : Bool -> DatePicker -> DatePicker
setOpen open (DatePicker model) =
    DatePicker { model | open = open }


{-| Expose the current selected date.
-}
getDate : DatePicker -> Maybe Date
getDate (DatePicker model) =
    model.date


{-| Expose the current presets.
-}
getPresets : DatePicker -> List Preset
getPresets (DatePicker model) =
    model.presets


{-| Sets the current date for the datepicker.
-}
setDate : Maybe Date -> DatePicker -> DatePicker
setDate date (DatePicker model) =
    let
        newDate =
            case date of
                Just a ->
                    Just <| getNewDate model a

                Nothing ->
                    Nothing
    in
        DatePicker ({ model | date = newDate } |> updateInputText)


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


{-| Sets the placeholder for the datepicker.
-}
setPlaceholder : String -> DatePicker -> DatePicker
setPlaceholder placeholder (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | placeholder = placeholder }
    in
        DatePicker { model | settings = newSettings }


{-| Sets the name for the datepicker.
-}
setInputName : String -> DatePicker -> DatePicker
setInputName inputName (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputName = Just inputName }
    in
        DatePicker { model | settings = newSettings }


{-| Sets the id for the datepicker.
-}
setInputId : String -> DatePicker -> DatePicker
setInputId inputId (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputId = Just inputId }
    in
        DatePicker { model | settings = newSettings }


{-| Sets the input icon for the datepicker.
-}
setInputIcon : Html Msg -> DatePicker -> DatePicker
setInputIcon inputIcon (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputIcon = Just inputIcon }
    in
        DatePicker { model | settings = newSettings }


{-| Sets the input attributes for the datepicker.
-}
setInputAttributes : List (Html.Attribute Msg) -> DatePicker -> DatePicker
setInputAttributes inputAttributes (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputAttributes = inputAttributes }
    in
        DatePicker { model | settings = newSettings }


{-| Sets the preset options for the datepicker.
-}
setPresetOptions : PresetOptions -> DatePicker -> DatePicker
setPresetOptions presetOptions (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | presetOptions = presetOptions }
    in
        DatePicker { model | settings = newSettings }


{-| Sets the restricted date range for the datepicker.
-}
setRestrictedDateRange : RestrictedDateRange -> DatePicker -> DatePicker
setRestrictedDateRange restrictedDateRange (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | restrictedDateRange = restrictedDateRange }
    in
        DatePicker { model | settings = newSettings }


{-| The daterange picker view. The date range passed is whatever date range it should treat as selected.
-}
view : DatePicker -> Html Msg
view (DatePicker ({ open, settings } as model)) =
    let
        potentialInputId =
            settings.inputId
                |> Maybe.map Attrs.id
                |> (List.singleton >> List.filterMap identity)

        icon =
            case settings.inputIcon of
                Just icn ->
                    icn

                Nothing ->
                    i [] []

        dateInput =
            div
                ([ Attrs.name <| Maybe.withDefault "" settings.inputName
                 , Events.onClick Click
                 , Attrs.class "elm-fancy-daterangepicker--date-input"
                 ]
                    ++ settings.inputAttributes
                    ++ potentialInputId
                )
                [ icon
                , text <| Maybe.withDefault settings.placeholder model.inputText
                ]
    in
        div [ Attrs.class "elm-fancy-daterangepicker--container" ]
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
                    renderCalendar model

                True ->
                    renderPresets model

        header =
            renderHeader
    in
        div
            [ Attrs.class "elm-fancy-daterangepicker--wrapper"
            , onPicker "mousedown" MouseDown
            , onPicker "mouseup" MouseUp
            ]
            [ header
            , content
            ]


{-| An opaque function that prints the datepicker calendar.
-}
renderCalendar : Model -> Html Msg
renderCalendar model =
    div [ Attrs.class "elm-fancy-daterangepicker--calendar" ] <|
        List.concat
            [ renderYearHeader model
            , renderQuarters model
            ]


{-| An opaque function gets the Html Msg for the header of the daterange picker.
-}
renderHeader : Html Msg
renderHeader =
    div [ Attrs.class "elm-fancy-daterangepicker--header" ]
        [ div [ Events.onClick Done, Attrs.class "elm-fancy-daterangepicker--done-btn" ] [ i [ Attrs.class "fa fa-check" ] [], text "Done" ]
        , div [ Events.onClick TogglePresets, Attrs.class "elm-fancy-daterangepicker--presets-btn" ] [ i [ Attrs.class "fa fa-cog" ] [], text "Presets" ]
        , div [ Events.onClick Reset, Attrs.class "elm-fancy-daterangepicker--reset-btn" ] [ i [ Attrs.class "fa fa-ban" ] [], text "Reset" ]
        ]


{-| An opaque function that gets the Html Msg for the presets of the daterange picker.
-}
renderPresets : Model -> Html Msg
renderPresets model =
    div [ Attrs.class "elm-fancy-daterangepicker--presets" ] <|
        if List.length model.presets > 0 then
            List.map (renderPreset model) model.presets
        else
            noPresets


{-| An opaque function that gets the Html Msg for a given preset.
-}
renderPreset : Model -> Preset -> Html Msg
renderPreset model preset =
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

        classString =
            mkClassString
                [ "elm-fancy-daterangepicker--preset"
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledPreset
                ]
    in
        div [ Attrs.class classString, setDate ]
            [ span [ Attrs.class "elm-fancy-daterangepicker--preset-name" ] [ text preset.name ]
            , span [ Attrs.class "elm-fancy-daterangepicker--preset-value" ] [ text <| model.settings.formatDate preset.date ]
            ]


{-| An opaque function that gets the year header Html Msg for the calendar.
-}
renderYearHeader : Model -> List (Html Msg)
renderYearHeader model =
    let
        date =
            mkDate model.currentYear.year Jan 1

        isDisabledYear =
            isDisabledDate model.enabledDateRange date

        setDate =
            case isDisabledYear of
                True ->
                    Events.onClick DoNothing

                False ->
                    Events.onClick <|
                        SetDate date

        yrLabelClassString =
            mkClassString
                [ "elm-fancy-daterangepicker--yr-btn"
                , "elm-fancy-daterangepicker--yr-label"
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledYear
                ]
    in
        [ div [ Attrs.class "elm-fancy-daterangepicker--yr-label-wrapper" ]
            [ div [ Attrs.class "elm-fancy-daterangepicker--yr-btn elm-fancy-daterangepicker--yr-prev", Events.onClick PrevYear ] []
            , div [ Attrs.class yrLabelClassString, setDate ] [ text model.currentYear.name ]
            , div [ Attrs.class "elm-fancy-daterangepicker--yr-btn elm-fancy-daterangepicker--yr-next", Events.onClick NextYear ] []
            ]
        ]


{-| An opaque function that gets the Html Msg for the quarters of the calendar.
-}
renderQuarters : Model -> List (Html Msg)
renderQuarters model =
    let
        quarters =
            model.currentYear.quarters
    in
        List.map (renderQuarter model) quarters


{-| An opaque function that gets the Html Msg for a given Quarter.
-}
renderQuarter : Model -> Quarter -> Html Msg
renderQuarter model qtr =
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

                                    setQtrDate =
                                        case isDisabledQtr of
                                            True ->
                                                Events.onClick DoNothing

                                            False ->
                                                Events.onClick <|
                                                    SetDate start

                                    classString =
                                        mkClassString
                                            [ "elm-fancy-daterangepicker--qtr-label"
                                            , mkClass "elm-fancy-daterangepicker--disabled" isDisabledQtr
                                            ]

                                    qtrLabel =
                                        div [ Attrs.class classString, setQtrDate ] [ text qtr.name ]
                                in
                                    div [ Attrs.class "elm-fancy-daterangepicker--qtr-row" ] <|
                                        List.concat
                                            [ [ qtrLabel ]
                                            , List.map (renderMonth model) qtr.months
                                            ]

                            ( _, _ ) ->
                                text ""
                in
                    qtrDiv

            ( _, _ ) ->
                text ""


{-| An opaque function that gets the Html Msg for a given month of the calendar.
-}
renderMonth : Model -> List Date -> Html Msg
renderMonth model m =
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

                    setMonthDate =
                        case isDisabledMonth of
                            True ->
                                Events.onClick DoNothing

                            False ->
                                Events.onClick <|
                                    SetDate startOfMonth_

                    classString =
                        mkClassString
                            [ "elm-fancy-daterangepicker--month-label"
                            , mkClass "elm-fancy-daterangepicker--disabled" isDisabledMonth
                            ]

                    monthDiv =
                        div [ Attrs.class classString, setMonthDate ]
                            [ text <|
                                formatMonth <|
                                    month a
                            ]
                in
                    div [ Attrs.class "elm-fancy-daterangepicker--month" ] <|
                        List.concat
                            [ [ monthDiv ]
                            , renderDaysOfWeek
                            , days
                            , padMonthRight (42 - List.length days)
                            ]

            _ ->
                text ""


{-| An opaque function that gets the Html Msg for a Day.
-}
renderDay : Model -> Date -> Html Msg
renderDay model date =
    let
        isDisabledDate_ =
            isDisabledDate model.enabledDateRange date

        isSelectedDate_ =
            isSelectedDate model date

        isToday_ =
            isToday model date

        classString =
            mkClassString
                [ "elm-fancy-daterangepicker--day"
                , mkClass "elm-fancy-daterangepicker--today" isToday_
                , mkClass "elm-fancy-daterangepicker--selected-range" isSelectedDate_
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledDate_
                ]

        setDate =
            case isDisabledDate_ of
                True ->
                    Events.onClick DoNothing

                False ->
                    Events.onClick <| SetDate date
    in
        div [ Attrs.class classString, setDate ]
            [ text <|
                toString <|
                    day date
            ]


{-| An opaque function that checks if the given date is in the selected range.
-}
isSelectedDate : Model -> Date -> Bool
isSelectedDate model date =
    case model.date of
        Just a ->
            dateEqualTo a date

        Nothing ->
            False


{-| An opaque function that checks if the passed in date is today.
-}
isToday : Model -> Date -> Bool
isToday model date =
    dateEqualTo date model.today


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
                            if dateGreaterThanOrEqualTo date start then
                                date
                            else
                                start
                    in
                        newDate

                ( Nothing, Just end ) ->
                    let
                        newDate =
                            if dateLessThanOrEqualTo date end then
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


(!>) : Model -> List (Cmd Msg) -> ( DatePicker, Cmd Msg )
(!>) model cmds =
    ( DatePicker model, Cmd.batch cmds )
