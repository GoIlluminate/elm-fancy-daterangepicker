module DateRangePicker exposing
    ( Msg, DateRangePicker
    , init, update, isOpen, setOpen, view, getDateRange, setDateRange
    , Settings, defaultSettings, setSettings, setDateRangeFormat, setPlaceholder, setInputName, setInputId, setInputIcon, setInputAttributes, setPresetOptions, setRestrictedDateRange, formatDateRange, getMinDate, getMaxDate
    , PresetOptions, PresetOption(..), Preset, PresetSetting, PresetInterval(..), PresetRelativeToToday(..), defaultPresetOptions, defaultPresets, mkPresetFromDateRange, mkPresetFromDates, getPresets
    )

{-| A customizable daterangepicker component.

@docs Msg, DateRangePicker
@docs init, update, isOpen, setOpen, view, getDateRange, setDateRange


# Settings

@docs Settings, defaultSettings, setSettings, setDateRangeFormat, setPlaceholder, setInputName, setInputId, setInputIcon, setInputAttributes, setPresetOptions, setRestrictedDateRange, formatDateRange, getMinDate, getMaxDate


## Presets

@docs PresetOptions, PresetOption, Preset, PresetSetting, PresetInterval, PresetRelativeToToday, defaultPresetOptions, defaultPresets, mkPresetFromDateRange, mkPresetFromDates, getPresets

-}

import Date
    exposing
        ( Date
        , day
        , fromCalendarDate
        , month
        , year
        )
import DateRangePicker.Common
    exposing
        ( DateRange
        , RestrictedDateRange(..)
        , inRange
        , mkDateRange
        )
import DateRangePicker.Common.Internal
    exposing
        ( EnabledDateRange
        , FullYear
        , Quarter
        , isDisabledDate
        , mkClass
        , mkClassString
        , mkEnabledDateRangeFromRestrictedDateRange
        , noPresets
        , padMonthLeft
        , padMonthRight
        , prepareYear
        , renderDaysOfWeek
        )
import DateRangePicker.Date
    exposing
        ( dateEqualTo
        , dateGreaterThanOrEqualTo
        , dateLessThanOrEqualTo
        , endOfMonth
        , formatDate
        , formatMonth
        , initDate
        , startOfMonth
        )
import Html
    exposing
        ( Html
        , div
        , i
        , span
        , text
        )
import Html.Attributes as Attrs
import Html.Events
import Json.Decode as Json
import Task
import Time exposing (Month(..), Weekday(..))


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
    , inputIcon : Maybe (Html Msg)
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


{-| A type that represents a preset daterange.

  - _name_ = Name of the preset. i.e. "Past Month"
  - _dateRange_ = The daterange that is selected when selecting the preset.

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
            Date.add Date.Days -1 today

        end =
            Date.add Date.Days -1 today
    in
    mkPresetFromDates "Yesterday" start end


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
    mkPresetFromDates "Past Week" start end


{-| An opaque function for the default preset "Past Month"
-}
presetPastMonth : Date -> Preset
presetPastMonth today =
    let
        start =
            Date.add Date.Days 1 <| Date.add Date.Months -1 today

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
            Date.add Date.Days 1 <| Date.add Date.Years -1 today

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
            year <| Date.add Date.Years -1 today

        start =
            fromCalendarDate newYear Jan 1

        end =
            fromCalendarDate newYear Dec 31
    in
    mkPresetFromDates "Last Year" start end


{-| An opaque function for the default preset "Last Month"
-}
presetLastMonth : Date -> Preset
presetLastMonth today =
    let
        newMonth =
            Date.add Date.Months -1 today

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
    , inputIcon = Nothing
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
                                , currentYear = prepareYear date
                                , presets = presets
                                , enabledDateRange = enabledDateRange
                            }

                        newDateRange =
                            Maybe.map (\x -> getNewDateRange newModel x) model.dateRange
                    in
                    ( { newModel | dateRange = newDateRange }, Cmd.none )

                PrevYear ->
                    let
                        prevYear =
                            prepareYear <|
                                fromCalendarDate (model.currentYear.year - 1) Jan 1
                    in
                    ( { model | currentYear = prevYear }, Cmd.none )

                NextYear ->
                    let
                        nextYear =
                            prepareYear <|
                                fromCalendarDate (model.currentYear.year + 1) Jan 1
                    in
                    ( { model | currentYear = nextYear }, Cmd.none )

                SetDateRange dateRange ->
                    let
                        newDateRange =
                            getNewDateRange model dateRange
                    in
                    ( { model
                        | dateRange = Just newDateRange
                        , startDate = Nothing
                        , endDate = Nothing
                        , showPresets = False
                        , currentYear = prepareYear newDateRange.end
                        , open = False
                        , forceOpen = False
                        , hoveredDate = Nothing
                      }
                    , Cmd.none
                    )

                SetDate date ->
                    case ( model.startDate, model.endDate ) of
                        ( Just _, Just _ ) ->
                            ( { model
                                | startDate = Just date
                                , endDate = Nothing
                                , dateRange = Nothing
                              }
                            , Cmd.none
                            )

                        ( Just start, Nothing ) ->
                            let
                                dateRange =
                                    if dateLessThanOrEqualTo start date then
                                        Just <| mkDateRange start date

                                    else
                                        Nothing
                            in
                            case dateRange of
                                Just _ ->
                                    ( { model
                                        | endDate = Nothing
                                        , startDate = Nothing
                                        , dateRange = dateRange
                                        , open = False
                                        , forceOpen = False
                                        , hoveredDate = Nothing
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( { model
                                        | startDate = Just date
                                        , endDate = Nothing
                                        , dateRange = Nothing
                                      }
                                    , Cmd.none
                                    )

                        ( Nothing, Nothing ) ->
                            ( { model
                                | startDate = Just date
                                , dateRange = Nothing
                              }
                            , Cmd.none
                            )

                        ( _, _ ) ->
                            ( model, Cmd.none )

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
                    ( { model
                        | open = newOpen
                        , forceOpen = False
                        , currentYear = newYear
                      }
                    , Cmd.none
                    )

                MouseDown ->
                    ( { model | forceOpen = True }, Cmd.none )

                MouseUp ->
                    ( { model | forceOpen = False }, Cmd.none )

                Done ->
                    let
                        newModel =
                            { model | open = False, forceOpen = False }
                    in
                    case newModel.dateRange of
                        Just _ ->
                            ( newModel, Cmd.none )

                        Nothing ->
                            case newModel.startDate of
                                Just b ->
                                    let
                                        newDateRange =
                                            mkDateRange b b
                                    in
                                    ( { newModel
                                        | dateRange = Just newDateRange
                                        , startDate = Nothing
                                        , endDate = Nothing
                                        , hoveredDate = Nothing
                                        , currentYear = prepareYear newDateRange.end
                                        , showPresets = False
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( newModel, Cmd.none )

                Reset ->
                    ( { model
                        | dateRange = Nothing
                        , startDate = Nothing
                        , endDate = Nothing
                        , hoveredDate = Nothing
                        , showPresets = False
                        , open = False
                        , forceOpen = False
                      }
                    , initCmd
                    )

                TogglePresets ->
                    ( { model | showPresets = not model.showPresets }, Cmd.none )

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

        icon =
            Maybe.withDefault (i [] []) settings.inputIcon

        dateInput =
            div
                (List.concat
                    [ [ Attrs.name <| Maybe.withDefault "" settings.inputName
                      , Html.Events.onClick Click
                      , Attrs.class "elm-fancy-daterangepicker--date-input"
                      ]
                    , settings.inputAttributes
                    , potentialInputId
                    ]
                )
                [ icon
                , text <| Maybe.withDefault settings.placeholder model.inputText
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
            if model.showPresets then
                renderPresets model

            else
                getCalendar model
    in
    div
        [ Attrs.class "elm-fancy-daterangepicker--wrapper"
        , Html.Events.stopPropagationOn "mousedown" <| Json.succeed ( MouseDown, True )
        , Html.Events.stopPropagationOn "mousedown" <| Json.succeed ( MouseUp, True )
        ]
        [ getHeader
        , content
        ]


{-| An opaque function that prints the daterangepicker calendar.
-}
getCalendar : Model -> Html Msg
getCalendar model =
    div [ Attrs.class "elm-fancy-daterangepicker--calendar" ] <|
        List.concat
            [ renderYearHeader model
            , renderQuarters model
            ]


{-| An opaque function gets the Html Msg for the header of the daterange picker.
-}
getHeader : Html Msg
getHeader =
    div [ Attrs.class "elm-fancy-daterangepicker--header" ]
        [ div [ Html.Events.onClick Done, Attrs.class "elm-fancy-daterangepicker--done-btn" ] [ i [ Attrs.class "fa fa-check" ] [], text "Done" ]
        , div [ Html.Events.onClick TogglePresets, Attrs.class "elm-fancy-daterangepicker--presets-btn" ] [ i [ Attrs.class "fa fa-cog" ] [], text "Presets" ]
        , div [ Html.Events.onClick Reset, Attrs.class "elm-fancy-daterangepicker--reset-btn" ] [ i [ Attrs.class "fa fa-ban" ] [], text "Reset" ]
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
            isDisabledDate model.enabledDateRange preset.dateRange.start
                && isDisabledDate model.enabledDateRange preset.dateRange.end

        setDateRange_ =
            if isDisabledPreset then
                Html.Events.onClick DoNothing

            else
                Html.Events.onClick <| SetDateRange preset.dateRange

        classString =
            mkClassString
                [ "elm-fancy-daterangepicker--preset"
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledPreset
                ]
    in
    div [ Attrs.class classString, setDateRange_ ]
        [ span [ Attrs.class "elm-fancy-daterangepicker--preset-name" ] [ text preset.name ]
        , span [ Attrs.class "elm-fancy-daterangepicker--preset-value" ] [ text <| model.settings.formatDateRange preset.dateRange ]
        ]


{-| An opaque function that gets the year header Html Msg for the calendar.
-}
renderYearHeader : Model -> List (Html Msg)
renderYearHeader model =
    let
        start =
            fromCalendarDate model.currentYear.year Jan 1

        end =
            fromCalendarDate model.currentYear.year Dec 31

        isDisabledYear =
            isDisabledDate model.enabledDateRange start
                && isDisabledDate model.enabledDateRange end

        setYearRange =
            if isDisabledYear then
                Html.Events.onClick DoNothing

            else
                Html.Events.onClick <| SetDateRange <| mkDateRange start end

        yrLabelClassString =
            mkClassString
                [ "elm-fancy-daterangepicker--yr-btn"
                , "elm-fancy-daterangepicker--yr-label"
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledYear
                ]
    in
    [ div [ Attrs.class "elm-fancy-daterangepicker--yr-label-wrapper" ]
        [ div [ Attrs.class "elm-fancy-daterangepicker--yr-btn elm-fancy-daterangepicker--yr-prev", Html.Events.onClick PrevYear ] []
        , div [ Attrs.class yrLabelClassString, setYearRange ] [ text model.currentYear.name ]
        , div [ Attrs.class "elm-fancy-daterangepicker--yr-btn elm-fancy-daterangepicker--yr-next", Html.Events.onClick NextYear ] []
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

                                setQtrDateRange =
                                    if isDisabledQtr then
                                        Html.Events.onClick DoNothing

                                    else
                                        Html.Events.onClick <| SetDateRange <| mkDateRange start end

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

                setMonthDateRange =
                    if isDisabledMonth then
                        Html.Events.onClick DoNothing

                    else
                        Html.Events.onClick <| SetDateRange <| mkDateRange (startOfMonth a) (endOfMonth a)

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

        setDate_ =
            if isDisabledDate_ then
                Html.Events.onClick DoNothing

            else
                Html.Events.onClick <| SetDate date

        hoverDate =
            Html.Events.onMouseOver <| HoverDay date
    in
    div [ Attrs.class classString, setDate_, hoverDate ]
        [ text <|
            String.fromInt <|
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
                    if dateGreaterThanOrEqualTo hoveredDate startDate then
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
            dateEqualTo a date || dateEqualTo b date

        ( Just a, _ ) ->
            dateEqualTo a date

        ( _, Just b ) ->
            dateEqualTo b date

        ( _, _ ) ->
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
    String.concat
        [ formatDate dateRange.start
        , " - "
        , formatDate dateRange.end
        ]
