module DateRangePicker exposing
    ( CalendarType(..)
    , Config
    , CustomPreset
    , Format(..)
    , Interval(..)
    , LanguageConfig
    , Model
    , Msg
    , PosixRange
    , PresetType(..)
    , Selection(..)
    , englishLanguageConfig
    , fullFormatter
    , getLocalSelection
    , getLocalSelectionRange
    , getUtcSelection
    , getUtcSelectionRange
    , initModel
    , initModelWithOptions
    , openDateRangePicker
    , presetToDisplayString
    , presetToPosixRange
    , setCalendarType
    , singleFormatter
    , subscriptions
    , update
    , view
    )

{-| A customizable date picker component.


# Basics

@docs Msg, Model, subscriptions, view, update

@docs initModel, openDateRangePicker


# Selection

@docs Selection, Format, PosixRange


# Settings

@ docs Config, LanguageConfig, englishLanguageConfig, initModelWithOptions, PresetType, Interval, CustomPreset, CalendarType


# Helpers

@docs setCalendarType, presetToDisplayString, presetToPosixRange

-}

import Browser.Dom as Dom exposing (Element, Error, getElement)
import Browser.Events
import Date exposing (Date)
import DateFormat
import DateFormat.Language as DateFormat
import DateRangePicker.DateRecordParser exposing (DateParts, DateTimeParts, Input(..), InputDate(..), YearAndMonth, datePartsToPosix, dateTimePartsToPosix, parseDateTime, yearAndMonthToPosix, yearToPosix)
import DateRangePicker.Helper exposing (onClickNoDefault)
import Derberos.Date.Calendar exposing (getCurrentMonthDatesFullWeeks, getFirstDayOfMonth, getFirstDayOfYear, getLastDayOfMonth, getLastDayOfYear)
import Derberos.Date.Core exposing (DateRecord, addTimezoneMilliseconds, adjustMilliseconds, civilToPosix, posixToCivil)
import Derberos.Date.Delta exposing (addDays, addMonths, addYears, nextWeekdayFromTime, prevWeekdayFromTime)
import Derberos.Date.Utils exposing (monthToNumber1)
import Html exposing (Attribute, Html, button, div, input, table, tbody, td, text, thead)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Json
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Events exposing (Event(..))
import Return2 as R2
import SelectList exposing (SelectList)
import Svg exposing (g, svg)
import Svg.Attributes as Svg
import Task
import Time exposing (Month(..), Posix, Weekday(..), Zone, posixToMillis, utc)


{-| An opaque type representing messages that are passed inside the DatePicker.
-}
type Msg
    = DoNothing
    | Open
    | Close
    | PrevCalendarRange PosixRange
    | NextCalendarRange PosixRange
    | SetSelection InternalSelection
    | OnInputFinish Posix
    | OnInputChange String
    | Reset
    | StartSelection Posix
    | EndSelection (Maybe Posix)
    | KeyDown Posix Zone RawKey
    | KeyUp RawKey
    | TerminateBadState
    | CancelShift
    | OnHoverOverDay Posix
    | OnMouseMove Mouse.Event
    | SetMouseOutside Bool
    | OnGetElementSuccess (Result Error Element)
    | CheckToMoveToNextVisibleRange Posix Zone
    | SetPresetMenu Bool
    | SelectPreset PresetType Posix


type MousePosition
    = Inside
    | OutsideTop
    | OutsideLeft
    | OutsideRight
    | OutsideBottom


type alias DateRange =
    { start : Date, end : Date }


{-| The type of that represents a range of two posix times.
-}
type alias PosixRange =
    { start : Posix, end : Posix }


{-| The type of that represents available presets that can be put in a preset menu.

There are a couple premade presets, but you can create a CustomPreset of your own as well.

-}
type PresetType
    = Today
    | Yesterday
    | PastWeek
    | PastMonth
    | PastYear
    | Custom CustomPreset


{-| The type of interval for a custom preset
-}
type Interval
    = Days
    | Months
    | Weeks
    | Years


{-| A record that represents a user created custom preset

A custom preset represents how to select a range of posix given today's date.

-}
type alias CustomPreset =
    { intervalStart : Interval
    , intervalStartValue : Int
    , intervalEnd : Interval
    , intervalEndValue : Int
    , display : String
    }


type InternalSelection
    = SingleSelection Format Posix
    | RangeSelection Format PosixRange
    | Unselected
    | Selecting PosixRange
    | PresetSelection PresetType


{-| The type which represents what the current selection is in the datepicker.

If you select a preset you can use @presetToPosixRange to get the appropriate posix range for the selection.

-}
type Selection
    = Single Format Posix
    | Range Format PosixRange
    | Preset PresetType


{-| The type which specifies what size calendar you want to display
-}
type CalendarType
    = FullCalendar
    | ThreeMonths
    | TwoMonths
    | OneMonth


{-| The type which specifies if a user had specified a time in the input box as well as the selected date.
-}
type Format
    = DateFormat
    | DateTimeFormat


{-| A record which represents the main datepicker model
-}
type alias Model =
    { selection : InternalSelection
    , availableForSelectionStart : Date
    , availableForSelectionEnd : Date
    , visibleCalendarRange : Maybe PosixRange
    , isMouseDown : Bool
    , isShiftDown : Bool
    , presets : List PresetType
    , calendarType : CalendarType
    , isOpen : Bool
    , inputText : String
    , terminationCounter : Int
    , currentlyHoveredDate : Maybe Posix
    , mousePosition : Maybe Mouse.Event
    , uiElement : Maybe Element
    , isMouseOutside : Bool
    , languageConfig : LanguageConfig
    , isPresetMenuOpen : Bool
    , keyboardSelectedPreset : Maybe (SelectList PresetType)
    }


{-| Initialize the datepicker with the default settings
-}
initModel : Model
initModel =
    { selection = Unselected
    , availableForSelectionStart = Date.fromCalendarDate 1900 Jan 1
    , availableForSelectionEnd = Date.fromCalendarDate 2100 Jan 1
    , visibleCalendarRange = Nothing
    , isMouseDown = False
    , isShiftDown = False
    , presets = []
    , calendarType = FullCalendar
    , isOpen = False
    , inputText = ""
    , terminationCounter = 0
    , currentlyHoveredDate = Nothing
    , mousePosition = Nothing
    , uiElement = Nothing
    , isMouseOutside = False
    , languageConfig = englishLanguageConfig
    , isPresetMenuOpen = False
    , keyboardSelectedPreset = Nothing
    }


{-| A record which specifies config options which can be set when initializes the datepicker
-}
type alias Config =
    { availableForSelectionStart : Date
    , availableForSelectionEnd : Date
    , presets : List PresetType
    , calendarType : CalendarType
    , isOpen : Bool
    , languageConfig : LanguageConfig
    }


{-| A record that can be used if a language other than english is wanted.
-}
type alias LanguageConfig =
    { done : String
    , reset : String
    , inputPlaceholder : String
    , presets : String
    , today : String
    , yesterday : String
    , pastWeek : String
    , pastMonth : String
    , pastYear : String
    , dateFormatLanguage : DateFormat.Language
    }


{-| The default english language
-}
englishLanguageConfig : LanguageConfig
englishLanguageConfig =
    { done = "Done"
    , reset = "Reset"
    , inputPlaceholder = "Start date - End date"
    , presets = "Presets"
    , today = "Today"
    , yesterday = "Yesterday"
    , pastWeek = "Past Week"
    , pastMonth = "Past Month"
    , pastYear = "Past Year"
    , dateFormatLanguage = DateFormat.english
    }


{-| Initialize the datepicker with the custom settings
-}
initModelWithOptions : Config -> Model
initModelWithOptions config =
    { initModel
        | availableForSelectionStart = config.availableForSelectionStart
        , availableForSelectionEnd = config.availableForSelectionEnd
        , presets = config.presets
        , calendarType = config.calendarType
        , isOpen = config.isOpen
    }


{-| A helper attribute which allows you to open the datepicker using any html element.

    button [ openDateRangePicker ] [ text "Open Me!" ]

You will need to call convert the message to the appropriate type via Html.map

-}
openDateRangePicker : Attribute Msg
openDateRangePicker =
    Html.Events.onClick Open


{-| The subscriptions for the datepicker

    Sub.map DatePickerMsgs <|
        DateRangePicker.subscriptions model.datePicker currentTime localZone

-}
subscriptions : Model -> Posix -> Zone -> Sub Msg
subscriptions model today zone =
    let
        shiftSubs =
            if model.isShiftDown then
                [ Keyboard.ups KeyUp
                , Browser.Events.onVisibilityChange (always CancelShift)
                , Time.every 100 (always TerminateBadState)
                ]

            else
                []

        keyDowns =
            [ Keyboard.downs (KeyDown today zone) ]

        mouseSubs =
            if model.isMouseDown then
                [ Browser.Events.onMouseUp (EndSelection model.currentlyHoveredDate |> Json.succeed)
                , Browser.Events.onVisibilityChange (EndSelection model.currentlyHoveredDate |> always)
                , Time.every 1250 (always <| CheckToMoveToNextVisibleRange today zone)
                ]

            else
                []
    in
    if model.isOpen then
        List.concat [ shiftSubs, mouseSubs, keyDowns ] |> Sub.batch

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            R2.withNoCmd model

        Open ->
            R2.withCmds
                [ Task.attempt OnGetElementSuccess <|
                    getElement "elm-fancy--daterangepicker-calendar"
                , Task.attempt (always DoNothing) <|
                    Dom.focus "elm-fancy--daterangepicker--input"
                ]
                { model | isOpen = True }

        Close ->
            R2.withNoCmd { model | isOpen = False }

        PrevCalendarRange currentVisibleRange ->
            updateCalendarRange model -1 currentVisibleRange

        NextCalendarRange currentVisibleRange ->
            updateCalendarRange model 1 currentVisibleRange

        SetSelection selection ->
            R2.withNoCmd { model | selection = selection, inputText = prettyFormatSelection selection model.languageConfig }

        OnInputFinish today ->
            let
                parseOutput =
                    parseDateTime
                        (List.map (\p -> presetToDisplayString p model.languageConfig) model.presets)
                        model.languageConfig.dateFormatLanguage
                        model.inputText

                updatedModel =
                    case parseOutput of
                        Ok value ->
                            -- todo how to do pretty format with time
                            let
                                selection =
                                    convertInput value model
                            in
                            { model
                                | selection = selection
                                , inputText = prettyFormatSelection selection model.languageConfig
                                , visibleCalendarRange = getVisibleRangeFromSelection selection model.calendarType today
                            }

                        Err _ ->
                            { model | inputText = prettyFormatSelection model.selection model.languageConfig }
            in
            R2.withNoCmd updatedModel

        OnInputChange newText ->
            R2.withNoCmd { model | inputText = newText }

        Reset ->
            R2.withNoCmd { model | inputText = "", selection = Unselected, visibleCalendarRange = Nothing }

        StartSelection posix ->
            R2.withNoCmd
                { model
                    | isMouseDown = True
                    , selection = Selecting { start = posix, end = posix }
                }

        EndSelection posix ->
            case posix of
                Just p ->
                    let
                        selection =
                            RangeSelection DateFormat <| normalizeSelectingRange <| createSelectingRange model p
                    in
                    R2.withNoCmd
                        { model
                            | isMouseDown = False
                            , selection = selection
                            , inputText = prettyFormatSelection selection model.languageConfig
                        }

                Nothing ->
                    R2.withNoCmd model

        KeyDown today zone rawKey ->
            onKey rawKey model (onKeyDown model today zone)

        KeyUp rawKey ->
            onKey rawKey
                model
                (\key ->
                    if key == Shift then
                        cancelShift model

                    else
                        R2.withNoCmd model
                )

        TerminateBadState ->
            -- todo what should dates be set to on terminate
            if model.terminationCounter < 0 then
                R2.withNoCmd
                    { model
                        | isShiftDown = False
                        , isMouseDown = False

                        --                    , endDate = Maybe.map .end model.dateRange
                        --                    , startDate = Maybe.map .start model.dateRange
                        , terminationCounter = 10
                    }

            else
                R2.withNoCmd { model | terminationCounter = model.terminationCounter - 1 }

        CancelShift ->
            cancelShift model

        OnHoverOverDay posix ->
            let
                selection =
                    if model.isMouseDown || model.isShiftDown then
                        Selecting <| createSelectingRange model posix

                    else
                        model.selection
            in
            R2.withNoCmd { model | currentlyHoveredDate = Just posix, selection = selection }

        OnMouseMove event ->
            R2.withNoCmd { model | mousePosition = Just event }

        SetMouseOutside bool ->
            R2.withNoCmd { model | isMouseOutside = bool }

        OnGetElementSuccess result ->
            case result of
                Ok element ->
                    R2.withNoCmd { model | uiElement = Just element }

                Err _ ->
                    R2.withNoCmd model

        CheckToMoveToNextVisibleRange today zone ->
            let
                visibleRange =
                    calcRange today utc model
            in
            case ( model.uiElement, model.mousePosition, model.isMouseOutside ) of
                ( Just element, Just position, True ) ->
                    case calculateMousePosition element position of
                        OutsideRight ->
                            updateCalendarRange model 1 visibleRange

                        OutsideLeft ->
                            updateCalendarRange model -1 visibleRange

                        _ ->
                            R2.withNoCmd model

                _ ->
                    R2.withNoCmd model

        SetPresetMenu bool ->
            R2.withNoCmd { model | isPresetMenuOpen = bool, keyboardSelectedPreset = Nothing }

        SelectPreset presetType today ->
            selectPreset presetType today model


selectPreset : PresetType -> Posix -> Model -> ( Model, Cmd Msg )
selectPreset presetType today model =
    let
        selection =
            PresetSelection presetType
    in
    R2.withCmd
        (Task.attempt (always DoNothing) <| Dom.focus "elm-fancy--daterangepicker--done")
        { model
            | isPresetMenuOpen = False
            , selection = selection
            , visibleCalendarRange = getVisibleRangeFromSelection selection model.calendarType today
            , inputText = prettyFormatSelection selection model.languageConfig
            , keyboardSelectedPreset = Nothing
        }


onKeyDown : Model -> Posix -> Zone -> Key -> ( Model, Cmd Msg )
onKeyDown model today zone key =
    case key of
        Shift ->
            if model.isShiftDown then
                R2.withNoCmd
                    { model
                        | terminationCounter =
                            if model.terminationCounter >= 2 then
                                model.terminationCounter

                            else
                                model.terminationCounter + 1
                    }

            else
                -- add start selection on shift logic
                R2.withNoCmd { model | isShiftDown = True }

        Escape ->
            if model.isPresetMenuOpen then
                R2.withNoCmd { model | isPresetMenuOpen = False, keyboardSelectedPreset = Nothing }

            else
                R2.withNoCmd { model | isOpen = False }

        ArrowDown ->
            arrowMovement model 1 SelectList.fromList

        ArrowUp ->
            arrowMovement model -1 createSelectListWithLast

        Enter ->
            if model.isPresetMenuOpen then
                case model.keyboardSelectedPreset of
                    Just a ->
                        selectPreset (SelectList.selected a) today model

                    Nothing ->
                        R2.withNoCmd model

            else
                R2.withNoCmd model

        _ ->
            R2.withNoCmd model


createSelectListWithLast : List PresetType -> Maybe (SelectList PresetType)
createSelectListWithLast presets =
    Maybe.map SelectList.selectLast (SelectList.fromList presets)


arrowMovement : Model -> Int -> (List PresetType -> Maybe (SelectList PresetType)) -> ( Model, Cmd Msg )
arrowMovement model moveDropdownBy createNewSelectList =
    if model.isPresetMenuOpen then
        let
            updatedModel =
                case model.keyboardSelectedPreset of
                    Just selectList ->
                        { model | keyboardSelectedPreset = SelectList.selectBy moveDropdownBy selectList }

                    Nothing ->
                        { model | keyboardSelectedPreset = createNewSelectList model.presets }
        in
        R2.withNoCmd updatedModel

    else
        R2.withNoCmd model


calculateMousePosition : Element -> Mouse.Event -> MousePosition
calculateMousePosition element event =
    let
        ( mouseX, mouseY ) =
            event.clientPos
    in
    if mouseX > (element.element.x + element.element.width) then
        OutsideRight

    else if mouseX < element.element.x then
        OutsideLeft

    else if mouseY < element.element.y then
        OutsideTop

    else if mouseY > (element.element.y + element.element.height) then
        OutsideBottom

    else
        Inside


createSelectingRange : Model -> Posix -> PosixRange
createSelectingRange model changedValue =
    case model.selection of
        SingleSelection _ _ ->
            { start = changedValue, end = changedValue }

        RangeSelection _ posixRange ->
            { start = posixRange.start, end = changedValue }

        Unselected ->
            { start = changedValue, end = changedValue }

        Selecting posixRange ->
            { start = posixRange.start, end = changedValue }

        PresetSelection _ ->
            { start = changedValue, end = changedValue }


cancelShift : Model -> ( Model, Cmd Msg )
cancelShift model =
    case model.selection of
        Selecting posixRange ->
            let
                selection =
                    PosixRange posixRange.start posixRange.end
                        |> normalizeSelectingRange
                        |> RangeSelection DateFormat
            in
            R2.withNoCmd
                { model
                    | isShiftDown = False
                    , terminationCounter = 10
                    , selection = selection
                    , inputText = prettyFormatSelection selection model.languageConfig
                }

        _ ->
            R2.withNoCmd
                { model
                    | isShiftDown = False
                    , terminationCounter = 10
                }


onKey : RawKey -> Model -> (Key -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
onKey rawKey model onValidKey =
    case Keyboard.anyKeyOriginal rawKey of
        Just key ->
            onValidKey key

        Nothing ->
            R2.withNoCmd model


updateCalendarRange : Model -> Int -> PosixRange -> ( Model, Cmd Msg )
updateCalendarRange model intervalChange currentVisibleRange =
    let
        updateWithIntervalFunc intervalFunc range =
            R2.withNoCmd
                { model
                    | visibleCalendarRange =
                        Just <|
                            { start = intervalFunc intervalChange utc range.start
                            , end = intervalFunc intervalChange utc range.end
                            }
                }

        visibleRange =
            Maybe.withDefault currentVisibleRange model.visibleCalendarRange
    in
    case model.calendarType of
        FullCalendar ->
            updateWithIntervalFunc (\int _ time -> addYears int time) visibleRange

        ThreeMonths ->
            updateWithIntervalFunc addMonths visibleRange

        TwoMonths ->
            updateWithIntervalFunc addMonths visibleRange

        OneMonth ->
            updateWithIntervalFunc addMonths visibleRange

{-| The view for the datepicker. You will have to pass in the current time as well as the local zone and the datepicker model.

    DateRangePicker.view currentTime localZone datePicker

-}
view : Posix -> Zone -> Model -> Html Msg
view today zone model =
    let
        -- Adjust today with the timezone and then every other view below uses utc which does not adjust the time when manipulating it
        adjustedToday =
            addTimezoneMilliseconds zone today

        visibleRange =
            calcRange adjustedToday zone model

        mouseEvent =
            if model.isMouseDown && model.isMouseOutside then
                Mouse.onMove OnMouseMove

            else
                Attrs.class ""
    in
    if model.isOpen then
        div [ Attrs.class "elm-fancy--daterangepicker" ]
            [ div
                [ Attrs.class "close"
                , Html.Events.onClick Close
                , mouseEvent
                , Html.Events.onMouseLeave <| SetMouseOutside False
                , Html.Events.onMouseEnter <| SetMouseOutside True
                ]
                []
            , div [ Attrs.class "body" ]
                [ topBar model visibleRange adjustedToday zone
                , leftSelector visibleRange
                , rightSelector visibleRange
                , calendarView today model visibleRange
                , bottomBar model
                ]
            ]

    else
        text ""


presetsDisplay : Model -> Posix -> Html Msg
presetsDisplay model today =
    if List.isEmpty model.presets then
        div [] []

    else if model.isPresetMenuOpen then
        presetMenu model today

    else
        div [ Attrs.class "preset--open--wrapper" ]
            [ button [ Attrs.class "preset--open", Html.Events.onClick <| SetPresetMenu True ]
                [ text <| model.languageConfig.presets, downArrow ]
            ]


presetMenu : Model -> Posix -> Html Msg
presetMenu model today =
    let
        isElementSelected item =
            case model.keyboardSelectedPreset of
                Just a ->
                    SelectList.selected a == item

                Nothing ->
                    False

        classList item =
            Attrs.classList [ ( "menu-item", True ), ( "menu-item--keyboard", isElementSelected item ) ]
    in
    div [ Attrs.class "preset-menu--container" ]
        [ div [ Attrs.class "preset-menu--close", Html.Events.onClick <| SetPresetMenu False ]
            []
        , div [ Attrs.class "preset-menu--content" ] <|
            List.map
                (\p ->
                    div [ Html.Events.onClick <| SelectPreset p today, classList p ]
                        [ text <| presetToDisplayString p model.languageConfig ]
                )
                model.presets
        ]


bottomBar : Model -> Html Msg
bottomBar model =
    div [ Attrs.class "bottom-bar" ]
        [ button
            [ Attrs.id "elm-fancy--daterangepicker--done"
            , Attrs.class "done"
            , Html.Events.onClick Close
            ]
            [ text model.languageConfig.done ]
        , button [ Attrs.class "reset", Html.Events.onClick Reset ]
            [ text model.languageConfig.reset ]
        ]


leftSelector : PosixRange -> Html Msg
leftSelector visibleRange =
    div
        [ Attrs.class "prev-range-selector", onClick <| PrevCalendarRange visibleRange ]
        [ div [] [ text "❮" ] ]


rightSelector : PosixRange -> Html Msg
rightSelector visibleRange =
    div [ Attrs.class "next-range-selector", onClick <| NextCalendarRange visibleRange ]
        [ div [] [ text "❯" ] ]


topBar : Model -> PosixRange -> Posix -> Zone -> Html Msg
topBar model visibleRange today zone =
    let
        ( fullCalendarSelector, class ) =
            case model.calendarType of
                FullCalendar ->
                    ( div [ Attrs.class "full-calendar-selector", onClick <| SetSelection selection ]
                        [ text <| selectionText visibleRange ]
                    , "top-bar--full"
                    )

                _ ->
                    ( text "", "top-bar--partial" )

        selection =
            RangeSelection DateFormat { start = getFirstDayOfYear utc visibleRange.start, end = getLastDayOfYear utc visibleRange.start }
    in
    div [ Attrs.class class ]
        [ fullCalendarSelector
        , presetsDisplay model today
        , calendarInput model today
        ]


selectionText : PosixRange -> String
selectionText visibleRange =
    String.fromInt <| Time.toYear utc visibleRange.start


calendarInput : Model -> Posix -> Html Msg
calendarInput model today =
    div [ Attrs.class "calendar-input" ]
        [ input
            [ Keyboard.Events.on Keypress [ ( Enter, OnInputFinish today ) ]
            , Html.Events.onBlur <| OnInputFinish today
            , Html.Events.onInput OnInputChange
            , Attrs.id "elm-fancy--daterangepicker--input"
            , Attrs.placeholder model.languageConfig.inputPlaceholder
            , Attrs.value model.inputText
            ]
            []
        ]


convertInput : Input -> Model -> InternalSelection
convertInput input model =
    case input of
        SingleInput inputDate ->
            convertInputDate inputDate

        RangeInput start end ->
            combineInputToRange start end

        CustomDate selectedCustomDate ->
            let
                selectedPreset =
                    List.filter (\p -> presetToDisplayString p model.languageConfig == selectedCustomDate) model.presets
            in
            case List.head selectedPreset of
                Just preset ->
                    PresetSelection preset

                Nothing ->
                    Unselected


combineInputToRange : InputDate -> InputDate -> InternalSelection
combineInputToRange start end =
    let
        startSelection =
            convertInputDate start

        endSelection =
            convertInputDate end
    in
    case ( startSelection, endSelection ) of
        ( SingleSelection _ startPosix, SingleSelection _ endPosix ) ->
            RangeSelection DateTimeFormat { start = startPosix, end = endPosix }

        ( SingleSelection _ startPosix, RangeSelection _ endPosixRange ) ->
            RangeSelection DateTimeFormat { start = startPosix, end = endPosixRange.end }

        ( RangeSelection _ startPosixRange, SingleSelection _ endPosix ) ->
            RangeSelection DateTimeFormat { start = startPosixRange.start, end = endPosix }

        ( RangeSelection _ startPosixRange, RangeSelection _ endPosixRange ) ->
            RangeSelection DateFormat { start = startPosixRange.start, end = endPosixRange.end }

        _ ->
            Unselected


convertInputDate : InputDate -> InternalSelection
convertInputDate inputDate =
    case inputDate of
        JustYear year ->
            RangeSelection DateFormat <| yearToPosixRange year utc

        JustYearAndMonth yearAndMonth ->
            RangeSelection DateFormat <| yearAndMonthToPosixRange yearAndMonth utc

        FullDate dateParts ->
            RangeSelection DateFormat <| datePartsToPosixRange dateParts utc

        FullDateTime dateTimeParts ->
            SingleSelection DateTimeFormat <| dateTimePartsToPosix dateTimeParts utc


calendarView : Posix -> Model -> PosixRange -> Html Msg
calendarView today model visibleRange =
    case model.calendarType of
        FullCalendar ->
            yearCalendarView today model visibleRange

        ThreeMonths ->
            threeMonthCalendarView today model visibleRange

        TwoMonths ->
            twoMonthCalendarView today model visibleRange

        OneMonth ->
            oneMonthCalendarView today model visibleRange


yearCalendarView : Posix -> Model -> PosixRange -> Html Msg
yearCalendarView today model visibleRange =
    let
        quarter name startMonth endMonth =
            div
                [ posixRangeForMonths startMonth endMonth (Time.toYear utc visibleRange.start) utc
                    |> RangeSelection DateFormat
                    |> SetSelection
                    |> Html.Events.onClick
                , Attrs.class "selection-hover"
                ]
                [ text name ]

        quarters =
            div
                [ Attrs.class "quarters" ]
                [ quarter "Q1" Jan Mar, quarter "Q2" Apr Jun, quarter "Q3" Jul Sep, quarter "Q4" Oct Dec ]
    in
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "year-calendar" ]
        [ quarters
        , table []
            [ tbody [ Attrs.class "year" ] <|
                List.map (\m -> monthCalendarView m today model) (getMonthsFromRange 0 11 visibleRange getFirstDayOfYear)
            ]
        ]


threeMonthCalendarView : Posix -> Model -> PosixRange -> Html Msg
threeMonthCalendarView today model visibleRange =
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "month-calendar" ]
        [ table []
            [ tbody [ Attrs.class "three-month" ] <|
                List.map (\m -> monthCalendarView m today model) (getMonthsFromRange 0 2 visibleRange getFirstDayOfMonthStartOfDay)
            ]
        ]


twoMonthCalendarView : Posix -> Model -> PosixRange -> Html Msg
twoMonthCalendarView today model visibleRange =
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "month-calendar" ]
        [ table []
            [ tbody [ Attrs.class "two-month" ] <|
                List.map (\m -> monthCalendarView m today model) (getMonthsFromRange 0 1 visibleRange getFirstDayOfMonthStartOfDay)
            ]
        ]


oneMonthCalendarView : Posix -> Model -> PosixRange -> Html Msg
oneMonthCalendarView today model visibleRange =
    let
        posixMonth =
            getFirstDayOfMonthStartOfDay utc visibleRange.start
    in
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "month-calendar" ]
        [ table [] [ tbody [ Attrs.class "one-month" ] [ monthCalendarView posixMonth today model ] ]
        ]


getMonthsFromRange : Int -> Int -> PosixRange -> (Zone -> Posix -> Posix) -> List Posix
getMonthsFromRange start end visibleRange fn =
    List.map
        (\x ->
            addMonths x utc <| fn utc visibleRange.start
        )
    <|
        List.range start end


posixRangeForMonths : Month -> Month -> Int -> Zone -> PosixRange
posixRangeForMonths startMonth endMonth currentYear zone =
    let
        start =
            yearAndMonthToPosixRange { year = currentYear, month = monthToNumber1 startMonth } zone

        end =
            yearAndMonthToPosixRange { year = currentYear, month = monthToNumber1 endMonth } zone
    in
    { start = start.start, end = end.end }


monthCalendarView : Posix -> Posix -> Model -> Html Msg
monthCalendarView currentMonth today model =
    let
        selection =
            RangeSelection DateFormat { start = getFirstDayOfMonthStartOfDay utc currentMonth, end = getLastDayOfMonthEndOfDay utc currentMonth }
    in
    td []
        [ table []
            [ thead [ Attrs.class "month--header", onClick <| SetSelection selection ]
                [ text <| monthFormatter model.languageConfig utc currentMonth ]
            , tbody [ Attrs.class "month" ] <|
                List.map (\x -> dayCalendarView utc currentMonth x today model) <|
                    getCurrentMonthDatesFullWeeks utc currentMonth
            ]
        ]


dayCalendarView : Zone -> Posix -> Posix -> Posix -> Model -> Html Msg
dayCalendarView zone currentMonth currentDay today model =
    let
        monthOfDate =
            Time.toMonth zone

        wantedMonth =
            monthOfDate currentMonth

        contentIsInCorrectMonth =
            monthOfDate currentDay == wantedMonth

        ( hoverAttr, content ) =
            if contentIsInCorrectMonth then
                ( Html.Events.onMouseOver <| OnHoverOverDay currentDay
                , [ text <| String.fromInt <| Time.toDay utc currentDay ]
                )

            else
                ( Attrs.class "", [] )

        setDate =
            if model.isShiftDown || model.isMouseDown then
                Just currentDay |> EndSelection |> onClickNoDefault

            else
                StartSelection currentDay |> DateRangePicker.Helper.mouseDownNoDefault

        isSameDayOfSelection getPosixFromSelection =
            contentIsInCorrectMonth && (Maybe.withDefault False <| Maybe.map (\p -> isSameDay p currentDay) (getPosixFromSelection model.selection today zone))

        classList =
            Attrs.classList
                [ ( "day", True )
                , ( "selected-range", contentIsInCorrectMonth && isInSelectionRange currentDay model today zone )
                , ( "border-selection", isSameDayOfSelection selectionStart || isSameDayOfSelection selectionEnd )

                -- todo check if zone is correct
                , ( "today", isSameDay currentDay today )
                ]
    in
    td [ classList, setDate, hoverAttr ] content


normalizeSelectingRange : PosixRange -> PosixRange
normalizeSelectingRange posixRange =
    if posixToMillis posixRange.start > posixToMillis posixRange.end then
        { start = posixRange.end, end = posixRange.start }

    else
        posixRange


isInSelectionRange : Posix -> Model -> Posix -> Zone -> Bool
isInSelectionRange comparisonPosix model today localZone =
    let
        posixInMillis =
            posixToMillis comparisonPosix

        compareRange range =
            posixToMillis range.start <= posixInMillis && posixInMillis <= posixToMillis range.end
    in
    case model.selection of
        SingleSelection _ posix ->
            -- todo is this concept getting removed - nope, but needs fixing around?
            False

        RangeSelection _ posixRange ->
            compareRange posixRange

        Unselected ->
            False

        Selecting posixRange ->
            compareRange <| normalizeSelectingRange posixRange

        PresetSelection presetType ->
            compareRange <| presetToPosixRange presetType today localZone


selectionEnd : InternalSelection -> Posix -> Zone -> Maybe Posix
selectionEnd selection today localZone =
    -- todo try to combine all these things that are casing
    case selection of
        SingleSelection _ posix ->
            Nothing

        RangeSelection _ posixRange ->
            Just posixRange.end

        Unselected ->
            Nothing

        Selecting posixRange ->
            Just <| .end <| normalizeSelectingRange posixRange

        PresetSelection presetType ->
            Just <| .end <| presetToPosixRange presetType today localZone


selectionStart : InternalSelection -> Posix -> Zone -> Maybe Posix
selectionStart selection today localZone =
    case selection of
        SingleSelection _ posix ->
            Just posix

        RangeSelection _ posixRange ->
            Just posixRange.start

        Unselected ->
            Nothing

        Selecting posixRange ->
            Just <| .start <| normalizeSelectingRange posixRange

        PresetSelection presetType ->
            Just <| .start <| presetToPosixRange presetType today localZone


isSameDay : Posix -> Posix -> Bool
isSameDay posix1 posix2 =
    let
        civel1 =
            posixToCivil posix1

        civel2 =
            posixToCivil posix2
    in
    civel1.day == civel2.day && civel1.month == civel2.month && civel1.year == civel2.year


calcRange : Posix -> Zone -> Model -> PosixRange
calcRange today zone model =
    Maybe.withDefault (convertToRange today model.calendarType) model.visibleCalendarRange


convertToRange : Posix -> CalendarType -> PosixRange
convertToRange day calendarType =
    case calendarType of
        FullCalendar ->
            { start = getFirstDayOfYear utc day, end = getLastDayOfYear utc day }

        ThreeMonths ->
            { start = getFirstDayOfMonthStartOfDay utc <| addMonths -1 utc day
            , end = getLastDayOfMonthEndOfDay utc <| addMonths 1 utc day
            }

        TwoMonths ->
            { start = getFirstDayOfMonthStartOfDay utc <| addMonths -1 utc day, end = getLastDayOfMonthEndOfDay utc day }

        OneMonth ->
            { start = getFirstDayOfMonthStartOfDay utc day, end = getLastDayOfMonthEndOfDay utc day }


getVisibleRangeFromSelection : InternalSelection -> CalendarType -> Posix -> Maybe PosixRange
getVisibleRangeFromSelection selection calendarType today =
    case selection of
        SingleSelection _ posix ->
            Just { start = getStartOfDay posix, end = getEndOfDay posix }

        RangeSelection _ posixRange ->
            convertToRange posixRange.start calendarType
                |> Just

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Just <| presetToPosixRange presetType today utc


prettyFormatSelection : InternalSelection -> LanguageConfig -> String
prettyFormatSelection selection languageConfig =
    -- todo handling time zones
    case selection of
        SingleSelection format posix ->
            singleFormatter languageConfig format utc posix

        RangeSelection format posixRange ->
            if isSameDay posixRange.start posixRange.end && format /= DateTimeFormat then
                singleFormatter languageConfig format utc posixRange.start

            else
                fullFormatter languageConfig format utc posixRange.start posixRange.end

        Unselected ->
            ""

        Selecting _ ->
            ""

        PresetSelection presetType ->
            presetToDisplayString presetType languageConfig


singleFormatter : LanguageConfig -> Format -> Zone -> Posix -> String
singleFormatter language format =
    let
        timeParts =
            case format of
                DateFormat ->
                    []

                DateTimeFormat ->
                    [ DateFormat.text " "
                    , DateFormat.hourMilitaryFixed
                    , DateFormat.text ":"
                    , DateFormat.minuteFixed
                    ]
    in
    DateFormat.formatWithLanguage language.dateFormatLanguage
        ([ DateFormat.monthNameAbbreviated
         , DateFormat.text " "
         , DateFormat.dayOfMonthNumber
         , DateFormat.text ", "
         , DateFormat.yearNumber
         ]
            ++ timeParts
        )


monthFormatter : LanguageConfig -> Zone -> Posix -> String
monthFormatter language =
    DateFormat.formatWithLanguage language.dateFormatLanguage
        [ DateFormat.monthNameFull
        ]


fullFormatter : LanguageConfig -> Format -> Zone -> Posix -> Posix -> String
fullFormatter language format zone start end =
    singleFormatter language format zone start
        ++ " to "
        ++ singleFormatter language format zone end



-- Copied from Derberos.Date.Utils and edited to make sunday first day of the week


getCurrentMonthDatesFullWeeks : Zone -> Posix -> List Posix
getCurrentMonthDatesFullWeeks zone time =
    let
        firstDayOfMonth =
            time
                |> getFirstDayOfMonthStartOfDay zone
                |> prevWeekdayFromTime Sun zone

        lastDayOfMonth =
            time
                |> getLastDayOfMonthEndOfDay zone
                |> nextWeekdayFromTime Sat zone

        numberDaysInMonth =
            (posixToMillis lastDayOfMonth - posixToMillis firstDayOfMonth) // (1000 * 60 * 60 * 24)
    in
    List.range 0 numberDaysInMonth
        |> List.map (\delta -> addDays delta firstDayOfMonth)


yearToPosixRange : Int -> Zone -> PosixRange
yearToPosixRange year zone =
    let
        posix =
            yearToPosix year zone
    in
    { start = getStartOfDay <| getFirstDayOfYear zone posix --todo maybe change to zone?
    , end = getEndOfDay <| getLastDayOfYear zone posix
    }


yearAndMonthToPosixRange : YearAndMonth -> Zone -> PosixRange
yearAndMonthToPosixRange yearMonth zone =
    let
        posix =
            yearAndMonthToPosix yearMonth zone
    in
    { start = getFirstDayOfMonthStartOfDay zone posix
    , end = getLastDayOfMonthEndOfDay zone posix
    }


getLastDayOfMonthEndOfDay : Zone -> Posix -> Posix
getLastDayOfMonthEndOfDay zone =
    getLastDayOfMonth zone >> getEndOfDay


getFirstDayOfMonthStartOfDay : Zone -> Posix -> Posix
getFirstDayOfMonthStartOfDay zone =
    getFirstDayOfMonth zone >> getStartOfDay


datePartsToPosixRange : DateParts -> Zone -> PosixRange
datePartsToPosixRange dateParts zone =
    let
        posix =
            datePartsToPosix dateParts zone
    in
    { start = getStartOfDay posix, end = getEndOfDay posix }


getEndOfDay : Posix -> Posix
getEndOfDay posix =
    let
        dateRecord =
            posixToCivil posix

        updatedDateRecord =
            { dateRecord
                | hour = 23
                , minute = 59
                , second = 0
                , millis = 0
            }
    in
    --todo  maybe call |> adjustMilliseconds zone?
    civilToPosix updatedDateRecord


getStartOfDay : Posix -> Posix
getStartOfDay posix =
    let
        dateRecord =
            posixToCivil posix

        updatedDateRecord =
            { dateRecord
                | hour = 0
                , minute = 0
                , second = 0
                , millis = 0
            }
    in
    --todo  maybe call |> adjustMilliseconds zone?
    civilToPosix updatedDateRecord


{-| A helper function to change the calendar type on an existing model
-}
setCalendarType : CalendarType -> Model -> Model
setCalendarType calendarType model =
    { model | calendarType = calendarType }


getLocalSelection : Model -> Maybe Selection
getLocalSelection model =
    case model.selection of
        SingleSelection format pos ->
            Single format pos
                |> Just

        RangeSelection format range ->
            Range format { start = range.start, end = range.end }
                |> Just

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Preset presetType
                |> Just


getUtcSelection : Zone -> Model -> Maybe Selection
getUtcSelection zone model =
    let
        correctDate =
            adjustMilliseconds zone
    in
    case model.selection of
        SingleSelection format pos ->
            Single format (correctDate pos)
                |> Just

        RangeSelection format range ->
            Range format { start = correctDate range.start, end = correctDate range.end }
                |> Just

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Preset presetType
                |> Just


getUtcSelectionRange : Zone -> Posix -> Model -> Maybe PosixRange
getUtcSelectionRange zone today model =
    let
        correctDate =
            adjustMilliseconds zone
    in
    case model.selection of
        SingleSelection _ pos ->
            { start = correctDate pos, end = correctDate pos }
                |> Just

        RangeSelection _ range ->
            { start = correctDate range.start, end = correctDate range.end }
                |> Just

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            presetToPosixRange presetType today zone
                |> Just


getLocalSelectionRange : Posix -> Model -> Maybe PosixRange
getLocalSelectionRange today model =
    case model.selection of
        SingleSelection _ pos ->
            { start = pos, end = pos }
                |> Just

        RangeSelection _ range ->
            { start = range.start, end = range.end }
                |> Just

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            presetToPosixRange presetType today utc
                |> Just



--------------------------------------------------------------------------------------------
--            PRESETS
--------------------------------------------------------------------------------------------


{-| A helper function to get the display value for a given preset
-}
presetToDisplayString : PresetType -> LanguageConfig -> String
presetToDisplayString presetType language =
    case presetType of
        Today ->
            language.today

        Yesterday ->
            language.yesterday

        PastWeek ->
            language.pastWeek

        PastMonth ->
            language.pastMonth

        PastYear ->
            language.pastYear

        Custom customPreset ->
            customPreset.display


convertInterval : Interval -> Int -> Posix -> Zone -> Posix
convertInterval interval intervalValue today localZone =
    case interval of
        Days ->
            addDays intervalValue today

        Months ->
            addMonths intervalValue localZone today

        Weeks ->
            addDays (intervalValue * -7) today

        Years ->
            addYears intervalValue today


{-| A helper function to get the posix range for a given preset
-}
presetToPosixRange : PresetType -> Posix -> Zone -> PosixRange
presetToPosixRange presetType today localZone =
    case presetType of
        Today ->
            { start = getStartOfDay today, end = getEndOfDay today }

        Yesterday ->
            { start = getStartOfDay <| addDays -1 today, end = getEndOfDay <| addDays -1 today }

        PastWeek ->
            { start = getStartOfDay <| addDays -7 today, end = getEndOfDay today }

        PastMonth ->
            -- todo utc?
            { start = getStartOfDay <| addMonths -1 localZone today, end = getEndOfDay today }

        PastYear ->
            { start = getStartOfDay <| addYears -1 today, end = getEndOfDay today }

        Custom customPreset ->
            { start =
                convertInterval customPreset.intervalStart customPreset.intervalStartValue today localZone
                    |> getStartOfDay
            , end =
                convertInterval customPreset.intervalEnd customPreset.intervalEndValue today localZone
                    |> getEndOfDay
            }


downArrow : Html msg
downArrow =
    svg [ Svg.width "20", Svg.height "20", Svg.viewBox "0 0 30 30" ]
        [ g [ Svg.stroke "none", Svg.strokeWidth "1", Svg.fill "none", Svg.fillRule "evenodd", Svg.strokeLinecap "round", Svg.strokeLinejoin "round" ]
            [ g [ Svg.transform "translate(15.000000, 15.000000) scale(-1, 1) rotate(90.000000) translate(-15.000000, -15.000000) translate(12.000000, 9.000000)", Svg.stroke "currentColor", Svg.strokeWidth "2" ]
                [ Svg.polyline [ Svg.points "0 12 6 6 0 0" ]
                    []
                ]
            ]
        ]
