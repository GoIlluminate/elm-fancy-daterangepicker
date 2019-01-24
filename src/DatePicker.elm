module DatePicker exposing
    ( Msg, DatePicker
    , init, update, subscriptions, isOpen, setOpen, view, getDate, setDate, getToday
    , Settings, defaultSettings, setSettings, setDateFormat, setPlaceholder, setInputName, setInputId, setInputIcon, setInputAttributes, setPresetOptions, setRestrictedDateRange, setCalendarDisplay, setInputView
    , PresetOptions, PresetOption(..), Preset, PresetSetting, PresetInterval(..), PresetRelativeToToday(..), defaultPresetOptions, defaultPresets, mkPresetFromDate, getPresets
    )

{-| A customizable daterangepicker component.

@docs Msg, DatePicker
@docs init, update, subscriptions, isOpen, setOpen, view, getDate, setDate, getToday


# Settings

@docs Settings, defaultSettings, setSettings, setDateFormat, setPlaceholder, setInputName, setInputId, setInputIcon, setInputAttributes, setPresetOptions, setRestrictedDateRange, setCalendarDisplay, setInputView


## Presets

@docs PresetOptions, PresetOption, Preset, PresetSetting, PresetInterval, PresetRelativeToToday, defaultPresetOptions, defaultPresets, mkPresetFromDate, getPresets

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


{-| A type representing messages that are passed within the DatePicker.
-}
type Msg
    = InitCurrentDate Date
    | PrevCalendarRange
    | NextCalendarRange
    | SetDate Date
    | DoNothing
    | Click
    | MouseDown
    | MouseUp
    | Reset
    | Save
    | TogglePresets Tab
    | CancelClick


{-| The opaque model to be used within the DatePicker.
-}
type alias Model =
    { today : Date
    , inputText : Maybe String
    , open : Bool
    , forceOpen : Bool
    , date : Maybe Date
    , showPresets : Bool
    , presets : List Preset
    , enabledDateRange : Maybe EnabledDateRange
    , settings : Settings
    , selectedTab : Tab
    , calendarRange : CalendarRange
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

  - _name_ = The name that you want to give the preset. i.e. "Past Month"
  - _interval_ = The interval in which you want to add/subtract the value from today.
  - _presetRelativeToToday_ = whether it is a range from [past - present] (ToToday) or [present - future] (FromToday)
  - _value_ = the number of your @interval that you are adding/subtracting.


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

  - _name_ = Name of the preset. i.e. "Past Month"
  - _date_ = The date that is selected when selecting the preset.

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
                            Date.add Date.Days (value * -1) today

                        Months ->
                            Date.add Date.Months (value * -1) today

                        Years ->
                            Date.add Date.Years (value * -1) today

                AfterToday ->
                    case interval of
                        Days ->
                            Date.add Date.Days value today

                        Months ->
                            Date.add Date.Months value today

                        Years ->
                            Date.add Date.Years value today

                Today ->
                    today

                Tomorrow ->
                    Date.add Date.Days 1 today

                Yesterday ->
                    Date.add Date.Days -1 today
    in
    mkPresetFromDate name date


{-| A function that creates a Preset from a name and a date
-}
mkPresetFromDate : String -> Date -> Preset
mkPresetFromDate name date =
    { name = name
    , date = fromCalendarDate (year date) (month date) (day date)
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
    , date = fromCalendarDate (year today) (month today) (day today)
    }


{-| An opaque function for the default preset "Yesterday"
-}
presetYesterday : Date -> Preset
presetYesterday today =
    let
        yesterday =
            Date.add Date.Days -1 today
    in
    mkPresetFromDate "Yesterday" yesterday


{-| An opaque function for the default preset "Tomorrow"
-}
presetTomorrow : Date -> Preset
presetTomorrow today =
    let
        tomorrow =
            Date.add Date.Days 1 today
    in
    mkPresetFromDate "Tomorrow" tomorrow


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
    , calendarDisplay = FullCalendar
    , inputView = Nothing
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
    , date = Nothing
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
                                | today = fromCalendarDate (year date) (month date) (day date)
                                , presets = presets
                                , enabledDateRange = enabledDateRange
                                , calendarRange = prepareCalendarRange settings.calendarDisplay date
                            }

                        newDate =
                            Maybe.map (\x -> getNewDate newModel x) model.date
                    in
                    ( { newModel | date = newDate }, Cmd.none )

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

                SetDate date ->
                    let
                        newDate =
                            getNewDate model date
                    in
                    ( { model
                        | date = Just newDate
                        , showPresets = False
                        , calendarRange = prepareCalendarRange model.settings.calendarDisplay date
                        , open = False
                        , forceOpen = False
                      }
                    , Cmd.none
                    )

                Click ->
                    let
                        newCalendarRange =
                            case model.date of
                                Just a ->
                                    prepareCalendarRange model.settings.calendarDisplay a

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
                        | date = Nothing
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

                Save ->
                    ( { model
                        | open = False
                      }
                    , initCmd
                    )

                CancelClick ->
                    ( { model | open = False }, Cmd.none )

                DoNothing ->
                    ( model, Cmd.none )
    in
    ( DatePicker <| updateInputText updatedModel, cmds )


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


{-| Expose today's date.
-}
getToday : DatePicker -> Date
getToday (DatePicker model) =
    model.today


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
            Maybe.map (\a -> getNewDate model a) date
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


{-| Sets the CalendarDisplay for the datepicker
-}
setCalendarDisplay : CalendarDisplay -> DatePicker -> DatePicker
setCalendarDisplay calendarDisplay (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | calendarDisplay = calendarDisplay }

        newCalendarRange =
            prepareCalendarRange calendarDisplay <|
                case model.date of
                    Nothing ->
                        model.today

                    Just d ->
                        d
    in
    DatePicker { model | settings = newSettings, calendarRange = newCalendarRange }


{-| Sets the inputView function for the datepicker
-}
setInputView : Maybe (String -> List (Html Msg)) -> DatePicker -> DatePicker
setInputView fn (DatePicker model) =
    let
        settings =
            model.settings

        newSettings =
            { settings | inputView = fn }
    in
    DatePicker { model | settings = newSettings }


{-| Subscribes to a mouse click
-}
subscriptions : DatePicker -> Sub Msg
subscriptions (DatePicker model) =
    if model.open then
        Browser.Events.onClick (Json.succeed CancelClick)

    else
        Sub.none


{-| The daterange picker view. The date range passed is whatever date range it should treat as selected.
-}
view : DatePicker -> Html Msg
view (DatePicker ({ open, settings } as model)) =
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
            if model.showPresets then
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
        , Html.Events.stopPropagationOn "mouseup" <| Json.succeed ( MouseUp, True )
        , onClickNoDefault DoNothing
        ]
        [ renderHeader model
        , content
        , renderFooter model
        ]


{-| An opaque function that prints the datepicker calendar.
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
        [ renderDatePickerHeader model
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
            isDisabledDate model.enabledDateRange preset.date

        setDate_ =
            if isDisabledPreset then
                onClickNoDefault DoNothing

            else
                onClickNoDefault <|
                    SetDate preset.date

        classString =
            mkClassString
                [ "elm-fancy-daterangepicker--preset"
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledPreset
                ]
    in
    div [ Attrs.class classString, setDate_ ]
        [ span [ Attrs.class "elm-fancy-daterangepicker--preset-name" ] [ text preset.name ]
        , span [ Attrs.class "elm-fancy-daterangepicker--preset-value" ] [ text <| model.settings.formatDate preset.date ]
        ]


{-| An opaque function that gets the year header Html Msg for the calendar.
-}
renderDatePickerHeader : Model -> Html Msg
renderDatePickerHeader model =
    let
        ( start, end ) =
            ( model.calendarRange.start, model.calendarRange.end )

        isDisabledDateRange =
            isDisabledDate model.enabledDateRange start
                && isDisabledDate model.enabledDateRange end

        rangeLabelClassString =
            mkClassString
                [ "elm-fancy-daterangepicker--range-btn"
                , "elm-fancy-daterangepicker--range-label"
                , mkClass "elm-fancy-daterangepicker--disabled" isDisabledDateRange
                ]
    in
    div [ Attrs.class "elm-fancy-daterangepicker--yr-label-wrapper" ]
        [ div [ Attrs.class "elm-fancy-daterangepicker--yr-btn elm-fancy-daterangepicker--yr-prev", onClickNoDefault PrevCalendarRange ] [ text "❮" ]
        , div [ Attrs.class rangeLabelClassString ] [ text model.calendarRange.name ]
        , div [ Attrs.class "elm-fancy-daterangepicker--yr-btn elm-fancy-daterangepicker--yr-next", onClickNoDefault NextCalendarRange ] [ text "❯" ]
        ]


{-| An opaque function that gets the Html Msg for the quarters of the calendar.
-}
renderFullCalendarBody : Model -> List (Html Msg)
renderFullCalendarBody model =
    let
        quarters =
            chunksOfLeft 3 model.calendarRange.months
    in
    List.map (renderQuarter model) quarters


{-| An opaque function that gets the List (Html Msg) for the body of the datePicker for ThreeMonths
-}
renderThreeMonthsBody : Model -> List (Html Msg)
renderThreeMonthsBody model =
    [ renderQuarter model model.calendarRange.months ]


{-| An opaque function that gets the List (Html Msg) for the body of the datePicker for TwoMonths
-}
renderTwoMonthsBody : Model -> List (Html Msg)
renderTwoMonthsBody model =
    [ div [ Attrs.class "elm-fancy-daterangepicker--months-row" ] <|
        List.map (renderMonth model) model.calendarRange.months
    ]


{-| An opaque function that gets the List (Html Msg) for the body of the datePicker for OneMonth
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

                                setQtrDate =
                                    if isDisabledQtr then
                                        onClickNoDefault DoNothing

                                    else
                                        onClickNoDefault <|
                                            SetDate start

                                classString =
                                    mkClassString
                                        [ "elm-fancy-daterangepicker--qtr-label"
                                        , mkClass "elm-fancy-daterangepicker--disabled" isDisabledQtr
                                        ]

                                qtrLabel =
                                    div [ Attrs.class classString, setQtrDate ] [ text <| "Q" ++ (String.fromInt <| Date.quarter start) ]
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

                setMonthDate =
                    if isDisabledMonth then
                        onClickNoDefault DoNothing

                    else
                        onClickNoDefault <|
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

        setDate_ =
            if isDisabledDate_ then
                onClickNoDefault DoNothing

            else
                onClickNoDefault <| SetDate date
    in
    div [ Attrs.class classString, setDate_ ]
        [ div [ Attrs.class "elm-fancy-daterangepicker--bubble" ]
            [ text <|
                String.fromInt <|
                    day date
            ]
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
