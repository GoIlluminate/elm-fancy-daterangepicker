module DateRangePicker exposing
    ( Msg, DateRangePicker
    , init, update, subscriptions, isOpen, setOpen, view, getDateRange, setDateRange, getToday
    , Settings, defaultSettings, setSettings, setDateRangeFormat, setPlaceholder, setInputName, setInputId, setInputIcon, setInputAttributes, setPresetOptions, setRestrictedDateRange, formatDateRange, getMinDate, getMaxDate, setCalendarDisplay
    , PresetOptions, PresetOption(..), Preset, PresetSetting, PresetInterval(..), PresetRelativeToToday(..), defaultPresetOptions, defaultPresets, mkPresetFromDateRange, mkPresetFromDates, getPresets, setInputView
    )

{-| A customizable daterangepicker component.

@docs Msg, DateRangePicker
@docs init, update, subscriptions, isOpen, setOpen, view, getDateRange, setDateRange, getToday


# Settings

@docs Settings, defaultSettings, setSettings, setDateRangeFormat, setPlaceholder, setInputName, setInputId, setInputIcon, setInputAttributes, setPresetOptions, setRestrictedDateRange, formatDateRange, getMinDate, getMaxDate, setCalendarDisplay, setInputView


## Presets

@docs PresetOptions, PresetOption, Preset, PresetSetting, PresetInterval, PresetRelativeToToday, defaultPresetOptions, defaultPresets, mkPresetFromDateRange, mkPresetFromDates, getPresets

-}

import Browser.Events
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
        ( CalendarDisplay(..)
        , DateRange
        , RestrictedDateRange(..)
        , calendarDisplayToClassStr
        , inRange
        , mkDateRange
        )
import DateRangePicker.Common.Internal
    exposing
        ( CalendarRange
        , EnabledDateRange
        , Months
        , chunksOfLeft
        , isDisabledDate
        , mkClass
        , mkClassString
        , mkEnabledDateRangeFromRestrictedDateRange
        , noPresets
        , onClickNoDefault
        , padMonthLeft
        , padMonthRight
        , prepareCalendarRange
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
    | PrevCalendarRange
    | NextCalendarRange
    | SetDateRange DateRange
    | Save
    | SetDate Date
    | DoNothing
    | Click
    | MouseDown
    | MouseUp
    | Reset
    | TogglePresets Tab
    | HoverDay Date
    | CancelClick


{-| The opaque model to be used within the DateRangePicker.
-}
type alias Model =
    { today : Date
    , inputText : Maybe String
    , open : Bool
    , forceOpen : Bool
    , dateRange : Maybe DateRange
    , startDate : Maybe Date
    , endDate : Maybe Date
    , hoveredDate : Maybe Date
    , showPresets : Bool
    , presets : List Preset
    , enabledDateRange : Maybe EnabledDateRange
    , settings : Settings
    , selectedTab : Tab
    , calendarRange : CalendarRange
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


{-| A type representing what tab is selected.
-}
type Tab
    = Calendar
    | Presets


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
    , dateRange = Nothing
    , startDate = Nothing
    , endDate = Nothing
    , hoveredDate = Nothing
    , showPresets = False
    , presets = []
    , enabledDateRange = Nothing
    , settings = defaultSettings
    , selectedTab = Calendar
    , calendarRange = prepareCalendarRange FullCalendar initDate
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
                        , calendarRange = prepareCalendarRange model.settings.calendarDisplay newDateRange.start
                        , open = False
                        , forceOpen = False
                        , hoveredDate = Nothing
                      }
                    , Cmd.none
                    )

                Save ->
                    ( { model
                        | open = False
                      }
                    , initCmd
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
                        newCalendarRange =
                            case model.dateRange of
                                Just a ->
                                    prepareCalendarRange model.settings.calendarDisplay a.start

                                Nothing ->
                                    model.calendarRange

                        newOpen =
                            not model.open
                    in
                    ( { model
                        | open = newOpen
                        , forceOpen = False
                        , selectedTab = Calendar
                        , calendarRange = newCalendarRange
                      }
                    , Cmd.none
                    )

                MouseDown ->
                    ( { model | forceOpen = True }, Cmd.none )

                MouseUp ->
                    ( { model | forceOpen = False }, Cmd.none )

                Reset ->
                    ( { model
                        | dateRange = Nothing
                        , startDate = Nothing
                        , endDate = Nothing
                        , hoveredDate = Nothing
                        , showPresets = False
                        , forceOpen = False
                      }
                    , initCmd
                    )

                TogglePresets tab ->
                    ( { model
                        | showPresets = not model.showPresets
                        , selectedTab = tab
                      }
                    , Cmd.none
                    )

                HoverDay date ->
                    ( { model | hoveredDate = Just date }, Cmd.none )

                CancelClick ->
                    ( { model | open = False }, Cmd.none )

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
    if model.open then
        Browser.Events.onClick (Json.succeed CancelClick)

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
                  , onClickNoDefault Click
                  , Attrs.class "elm-fancy-daterangepicker--date-input"
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
    div [ Attrs.class "elm-fancy-daterangepicker--container" ]
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
        content =
            if model.selectedTab == Presets then
                renderPresets model

            else
                renderCalendar model

        tabClass =
            case model.selectedTab of
                Calendar ->
                    "elm-fancy-daterangepicker--calendar-tab"

                _ ->
                    "elm-fancy-daterangepicker--presets-tab"

        calendarDisplayClass =
            "elm-fancy-daterangepicker--" ++ calendarDisplayToClassStr model.settings.calendarDisplay
    in
    div
        [ Attrs.class "elm-fancy-daterangepicker--wrapper elm-fancy-daterangepicker--box-shadow"
        , Attrs.class tabClass
        , Attrs.class calendarDisplayClass
        , Html.Events.stopPropagationOn "mousedown" <| Json.succeed ( MouseDown, True )
        , Html.Events.stopPropagationOn "mousedown" <| Json.succeed ( MouseUp, True )
        , onClickNoDefault DoNothing
        ]
        [ renderHeader model
        , content
        , renderFooter model
        ]


{-| An opaque function that prints the daterangepicker calendar.
-}
renderCalendar : Model -> Html Msg
renderCalendar model =
    let
        calendarDisplayClass =
            "elm-fancy-daterangepicker--" ++ calendarDisplayToClassStr model.settings.calendarDisplay

        body =
            div [ Attrs.class "elm-fancy-daterangepicker--body--container" ] <|
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
        [ Attrs.class "elm-fancy-daterangepicker--calendar"
        , Attrs.class calendarDisplayClass
        ]
        [ renderDateRangePickerHeader model
        , body
        ]


{-| An opaque function gets the Html Msg for the header of the daterange picker.
-}
renderHeader : Model -> Html Msg
renderHeader model =
    let
        getSelectedClass b =
            if b then
                "selected"

            else
                ""
    in
    div [ Attrs.class "elm-fancy-daterangepicker--header" ]
        [ div
            [ onClickNoDefault <| TogglePresets Calendar
            , Attrs.class "elm-fancy-daterangepicker--presets-btn"
            , Attrs.class <| getSelectedClass <| model.selectedTab == Calendar
            ]
            [ text "Calendar" ]
        , div
            [ onClickNoDefault <| TogglePresets Presets
            , Attrs.class "elm-fancy-daterangepicker--presets-btn"
            , Attrs.class <| getSelectedClass <| model.selectedTab == Presets
            ]
            [ text "Presets" ]
        ]


{-| An opaque function gets the Html Msg for the footer of the daterange picker.
-}
renderFooter : Model -> Html Msg
renderFooter model =
    case model.selectedTab of
        Calendar ->
            div [ Attrs.class "elm-fancy-daterangepicker--footer" ]
                [ div [ Attrs.class "round-btns", onClickNoDefault Reset ] [ text "Reset" ]
                , div [ Attrs.class "round-btns", onClickNoDefault Save ] [ text "Save" ]
                ]

        _ ->
            span [] []


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
                onClickNoDefault DoNothing

            else
                onClickNoDefault <| SetDateRange preset.dateRange

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
                onClickNoDefault DoNothing

            else
                onClickNoDefault <| SetDateRange <| mkDateRange start end

        rangeLabelClassString =
            mkClassString
                [ "elm-fancy-daterangepicker--range-btn"
                , "elm-fancy-daterangepicker--range-label"
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledDateRange
                ]
    in
    div [ Attrs.class "elm-fancy-daterangepicker--yr-label-wrapper" ]
        [ div [ Attrs.class "elm-fancy-daterangepicker--range-btn elm-fancy-daterangepicker--range-prev", onClickNoDefault PrevCalendarRange ] [ text "❮" ]
        , div [ Attrs.class rangeLabelClassString, setRange ] [ text model.calendarRange.name ]
        , div [ Attrs.class "elm-fancy-daterangepicker--range-btn elm-fancy-daterangepicker--range-next", onClickNoDefault NextCalendarRange ] [ text "❯" ]
        ]


{-| An opaque function that gets the List (Html Msg) for the body of the dateRangePicker for FullCalendarYear
-}
renderFullCalendarBody : Model -> List (Html Msg)
renderFullCalendarBody model =
    let
        quarters =
            chunksOfLeft 3 model.calendarRange.months
    in
    List.map (renderQuarter model) quarters


{-| An opaque function that gets the List (Html Msg) for the body of the dateRangePicker for ThreeMonths
-}
renderThreeMonthsBody : Model -> List (Html Msg)
renderThreeMonthsBody model =
    [ renderQuarter model model.calendarRange.months ]


{-| An opaque function that gets the List (Html Msg) for the body of the dateRangePicker for TwoMonths
-}
renderTwoMonthsBody : Model -> List (Html Msg)
renderTwoMonthsBody model =
    [ div [ Attrs.class "elm-fancy-daterangepicker--months-row" ] <|
        List.map (renderMonth model) model.calendarRange.months
    ]


{-| An opaque function that gets the List (Html Msg) for the body of the dateRangePicker for OneMonth
-}
renderOneMonthBody : Model -> List (Html Msg)
renderOneMonthBody model =
    [ div [ Attrs.class "elm-fancy-daterangepicker--months-row" ] <|
        List.map (renderMonth model) model.calendarRange.months
    ]


{-| An opaque function that gets the Html Msg for a given Quarter.
-}
renderQuarter : Model -> Months -> Html Msg
renderQuarter model months =
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

                qtrDiv =
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
                                        onClickNoDefault <| SetDateRange <| mkDateRange start end

                                classString =
                                    mkClassString
                                        [ "elm-fancy-daterangepicker--qtr-label"
                                        , mkClass "elm-fancy-daterangepicker--disabled" isDisabledQtr
                                        ]

                                qtrLabel =
                                    div [ Attrs.class classString, setQtrDateRange ] [ text <| "Q" ++ (String.fromInt <| Date.quarter start) ]
                            in
                            div [ Attrs.class "elm-fancy-daterangepicker--qtr-row" ] <|
                                List.concat
                                    [ [ qtrLabel ]
                                    , List.map (renderMonth model) months
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
                        onClickNoDefault DoNothing

                    else
                        onClickNoDefault <| SetDateRange <| mkDateRange (startOfMonth a) (endOfMonth a)

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

        isStart_ =
            isStart model date

        isEnd_ =
            isEnd model date

        classString =
            mkClassString
                [ "elm-fancy-daterangepicker--day"
                , mkClass "elm-fancy-daterangepicker--today" isToday_
                , mkClass "elm-fancy-daterangepicker--selected-range" <| isSelectedDateRange_ && not isStart_ && not isEnd_
                , mkClass "elm-fancy-daterangepicker--hovered-range" isHoveredDateRange_
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledDate_
                , mkClass "elm-fancy-daterangepicker--start" isStart_
                , mkClass "elm-fancy-daterangepicker--end" isEnd_
                ]

        setDate_ =
            if isDisabledDate_ then
                onClickNoDefault DoNothing

            else
                onClickNoDefault <| SetDate date

        hoverDate =
            Html.Events.onMouseOver <| HoverDay date
    in
    div [ Attrs.class classString, setDate_, hoverDate ]
        [ div [ Attrs.class "elm-fancy-daterangepicker--bubble" ]
            [ text <|
                String.fromInt <|
                    day date
            ]
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
    case model.dateRange of
        Nothing ->
            case ( model.startDate, model.endDate ) of
                ( Just a, Just b ) ->
                    dateEqualTo a date || dateEqualTo b date

                ( Just a, _ ) ->
                    dateEqualTo a date

                ( _, Just b ) ->
                    dateEqualTo b date

                ( _, _ ) ->
                    False

        Just { start, end } ->
            dateEqualTo start date || dateEqualTo end date


{-| An opaque function that checks if the passed in date is equal
to the model's startDate
-}
isStart : Model -> Date -> Bool
isStart model date =
    case model.dateRange of
        Just { start } ->
            dateEqualTo start date

        Nothing ->
            case model.startDate of
                Just a ->
                    dateEqualTo a date

                Nothing ->
                    False


{-| An opaque function that checks if the passed in date is equal
to the model's endDate
-}
isEnd : Model -> Date -> Bool
isEnd model date =
    let
        fun =
            Maybe.andThen
                (\a ->
                    Maybe.map
                        (\sd ->
                            dateEqualTo a date && dateGreaterThanOrEqualTo a sd
                        )
                        model.startDate
                )
                model.hoveredDate
    in
    case model.dateRange of
        Just { end } ->
            dateEqualTo end date

        Nothing ->
            case model.endDate of
                Just a ->
                    dateEqualTo a date

                Nothing ->
                    Maybe.withDefault False fun


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
