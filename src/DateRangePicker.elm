module DateRangePicker exposing
    ( Msg, Model, subscriptions, view, update
    , open
    , Selection(..), Format(..), PosixRange
    , setCalendarType, presetToDisplayString
    , CalendarType(..), Config, CustomPreset, DatePickerType(..), Interval(..), LanguageConfig, PresetType(..), defaultConfig, defaultOpener, englishLanguageConfig, fullFormatter, getEndOfDay, getStartOfDay, hasLocalRangeChanged, hasLocalSelectionChanged, hasUtcRangeChanged, hasUtcSelectionChanged, init, initWithOptions, isOpen, languageConfig, localSelection, localSelectionRange, localSelectionSingle, presetToLocalPosixRange, presetToUtcPosixRange, presets, selectPreset, setOpen, setSelection, singleFormatter, updateModelWithConfig, utcSelection, utcSelectionRange, utcSelectionSingle
    )

{-| A customizable date picker component.


# Basics

@docs Msg, Model, subscriptions, view, update

@docs initModel, open


# Selection

@docs Selection, Format, PosixRange


# Settings

@ docs Config, LanguageConfig, englishLanguageConfig, initModelWithOptions, PresetType, Interval, CustomPreset, CalendarType, defaultConfig


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
import Svg exposing (g, path, svg)
import Svg.Attributes as Svg
import Task
import Time exposing (Month(..), Posix, Weekday(..), Zone, posixToMillis, utc)


{-| An opaque type representing messages that are passed inside the DatePicker.
-}
type Msg
    = DoNothing
    | Open String
    | Close Posix
    | SetVisibleRange PosixRange
    | SetSelection InternalSelection
    | OnInputFinish Posix
    | OnInputChange String
    | Reset
    | StartSelection Posix
    | EndSelection (Maybe Posix)
    | KeyDown Posix RawKey
    | KeyUp RawKey
    | TerminateBadState
    | CancelShift
    | OnHoverOverDay Posix
    | OnMouseMove Mouse.Event
    | SetMouseOutside Bool
    | OnGetElementSuccess (Result Error Element)
    | OnGetDatePickerButton (Result Error Element)
    | CheckToMoveToNextVisibleRange Posix
    | SetPresetMenu Bool
    | SelectPreset PresetType Posix
    | ToggleFormat


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
    = SingleSelection Posix
    | RangeSelection PosixRange
    | Unselected
    | Selecting PosixRange
    | PresetSelection PresetType


{-| The type which represents what the current selection is in the datepicker.

If you select a preset you can use @presetToPosixRange to get the appropriate posix range for the selection.

-}
type Selection
    = Single Posix
    | Range PosixRange
    | Preset PresetType


{-| The type which specifies what size calendar you want to display
-}
type CalendarType
    = FullCalendar
    | ThreeMonths
    | TwoMonths
    | OneMonth


type DatePickerType
    = DateRangePicker
    | DatePicker


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
    , uiButton : Maybe Element
    , isMouseOutside : Bool
    , languageConfig : LanguageConfig
    , isPresetMenuOpen : Bool
    , keyboardSelectedPreset : Maybe (SelectList PresetType)
    , displayFormat : Format
    , datePickerType : DatePickerType
    , hidePresets : Bool
    }


{-| Initialize the datepicker with the default settings
-}
init : Model
init =
    { selection = Unselected
    , availableForSelectionStart = Date.fromCalendarDate 1900 Jan 1
    , availableForSelectionEnd = Date.fromCalendarDate 2100 Dec 31
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
    , uiButton = Nothing
    , isMouseOutside = False
    , languageConfig = englishLanguageConfig
    , isPresetMenuOpen = False
    , keyboardSelectedPreset = Nothing
    , displayFormat = DateFormat
    , datePickerType = DateRangePicker
    , hidePresets = False
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
    , datePickerType : DatePickerType
    , hidePresets : Bool
    , defaultSelection : Maybe Selection
    }


{-| A record that can be used if a language other than english is wanted.
-}
type alias LanguageConfig =
    { done : String
    , reset : String
    , datePickerInputPlaceholder : String
    , dateRangePickerInputPlaceholder : String
    , presets : String
    , today : String
    , yesterday : String
    , pastWeek : String
    , pastMonth : String
    , pastYear : String
    , includeTimeTitle : String
    , dateFormatLanguage : DateFormat.Language
    }


{-| The default english language
-}
englishLanguageConfig : LanguageConfig
englishLanguageConfig =
    { done = "Done"
    , reset = "Reset"
    , datePickerInputPlaceholder = "Select a date"
    , dateRangePickerInputPlaceholder = "Select a date range"
    , presets = "Presets"
    , today = "Today"
    , yesterday = "Yesterday"
    , pastWeek = "Past Week"
    , pastMonth = "Past Month"
    , pastYear = "Past Year"
    , includeTimeTitle = "Include Time"
    , dateFormatLanguage = DateFormat.english
    }


{-| Initialize the datepicker with the custom settings
-}
initWithOptions : Config -> Model
initWithOptions config =
    updateModelWithConfig init config


{-| A default config which can be combined with @initWithOptions so that you only need to specify the fields which you want to customize
-}
defaultConfig : Config
defaultConfig =
    { availableForSelectionStart = Date.fromCalendarDate 1900 Jan 1
    , availableForSelectionEnd = Date.fromCalendarDate 2100 Dec 31
    , presets = []
    , calendarType = FullCalendar
    , isOpen = False
    , languageConfig = englishLanguageConfig
    , datePickerType = DateRangePicker
    , hidePresets = False
    , defaultSelection = Nothing
    }


{-| A helper attribute which allows you to open the datepicker using any html element.

    button [ open buttonId ] [ text "Open Me!" ]

You will need to call convert the message to the appropriate type via Html.map

-}
open : String -> Attribute Msg
open openerId =
    Html.Events.onClick (Open openerId)


defaultOpener : Model -> String -> Html Msg
defaultOpener model openerId =
    let
        selectionValue =
            prettyFormatSelection model.selection model.languageConfig model.displayFormat

        displayValue =
            if String.isEmpty selectionValue then
                inputPlaceHolder model

            else
                selectionValue
    in
    button
        [ Attrs.class "elm-fancy--daterangepicker--opener"
        , Keyboard.Events.on Keypress [ ( Enter, Open openerId ) ]
        , open openerId
        , Attrs.id openerId
        ]
        [ div [ Attrs.class "opener--content" ]
            [ calendarIcon
            , div [ Attrs.class "opener-text" ] [ text displayValue ]
            , downArrow
            ]
        ]


{-| The subscriptions for the datepicker

    Sub.map DatePickerMsgs <|
        DateRangePicker.subscriptions model.datePicker currentTime localZone

-}
subscriptions : Model -> Posix -> Zone -> Sub Msg
subscriptions model today zone =
    let
        adjustedToday =
            adjustMilliseconds zone today

        shiftSubs =
            if model.isShiftDown then
                [ Keyboard.ups KeyUp
                , Browser.Events.onVisibilityChange (always CancelShift)
                , Time.every 100 (always TerminateBadState)
                ]

            else
                []

        keyDowns =
            [ Keyboard.downs (KeyDown adjustedToday) ]

        mouseSubs =
            if model.isMouseDown then
                [ Browser.Events.onMouseUp (EndSelection model.currentlyHoveredDate |> Json.succeed)
                , Browser.Events.onVisibilityChange (EndSelection model.currentlyHoveredDate |> always)
                , Time.every 1250 (always <| CheckToMoveToNextVisibleRange adjustedToday)
                ]

            else
                []

        closeSub =
            if model.isMouseOutside && not model.isMouseDown then
                [ Browser.Events.onClick (Json.succeed <| Close today) ]

            else
                []
    in
    if model.isOpen then
        List.concat [ shiftSubs, mouseSubs, keyDowns, closeSub ] |> Sub.batch

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            R2.withNoCmd model

        Open buttonId ->
            R2.withCmds
                [ Task.attempt OnGetElementSuccess <|
                    getElement "elm-fancy--daterangepicker--wrapper"
                , Task.attempt (always DoNothing) <|
                    Dom.focus "elm-fancy--daterangepicker--input"
                , Task.attempt OnGetDatePickerButton <|
                    getElement buttonId
                ]
                { model | isOpen = True }

        Close today ->
            R2.withNoCmd <| finishInput today { model | isOpen = False, uiButton = Nothing, uiElement = Nothing, isMouseOutside = False }

        SetVisibleRange visibleCalendarRange ->
            R2.withNoCmd
                { model
                    | visibleCalendarRange = Just visibleCalendarRange
                }

        SetSelection selection ->
            let
                updatedSelection =
                    finalizeSelection model selection
            in
            R2.withNoCmd
                { model
                    | selection = updatedSelection
                    , inputText = prettyFormatSelection updatedSelection model.languageConfig model.displayFormat
                }

        OnInputFinish today ->
            R2.withNoCmd <| finishInput today model

        OnInputChange newText ->
            R2.withNoCmd { model | inputText = newText }

        Reset ->
            R2.withNoCmd { model | inputText = "", selection = Unselected, visibleCalendarRange = Nothing }

        StartSelection posix ->
            case model.datePickerType of
                DatePicker ->
                    let
                        selection =
                            SingleSelection posix
                    in
                    R2.withNoCmd
                        { model
                            | selection = selection
                            , inputText = prettyFormatSelection selection model.languageConfig model.displayFormat
                        }

                DateRangePicker ->
                    R2.withNoCmd
                        { model
                            | isMouseDown = True
                            , selection = Selecting { start = posix, end = posix }
                        }

        EndSelection posix ->
            case posix of
                Just date ->
                    let
                        selection =
                            date
                                |> getEndOfDay
                                |> createSelectingRange model
                                |> normalizeSelectingRange
                                |> RangeSelection
                                |> finalizeSelection model
                    in
                    R2.withNoCmd
                        { model
                            | isMouseDown = False
                            , selection = selection
                            , inputText = prettyFormatSelection selection model.languageConfig model.displayFormat
                        }

                Nothing ->
                    R2.withNoCmd model

        KeyDown today rawKey ->
            onKey rawKey model (onKeyDown model today)

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

        OnGetDatePickerButton result ->
            case result of
                Ok element ->
                    R2.withNoCmd { model | uiButton = Just element }

                Err _ ->
                    R2.withNoCmd model

        CheckToMoveToNextVisibleRange today ->
            let
                visibleRange =
                    calcRange today model
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

        ToggleFormat ->
            let
                updatedModel =
                    case model.displayFormat of
                        DateFormat ->
                            { model
                                | displayFormat = DateTimeFormat
                                , inputText = prettyFormatSelection model.selection model.languageConfig DateTimeFormat
                            }

                        DateTimeFormat ->
                            let
                                selection =
                                    finalizeSelection model model.selection
                            in
                            { model
                                | displayFormat = DateFormat
                                , inputText = prettyFormatSelection selection model.languageConfig DateFormat
                                , selection = selection
                            }
            in
            R2.withNoCmd updatedModel


selectionToInternalSelection : Maybe Selection -> InternalSelection
selectionToInternalSelection selection =
    Maybe.withDefault Unselected <|
        Maybe.map
            (\s ->
                case s of
                    Single val ->
                        SingleSelection val

                    Range val ->
                        RangeSelection val

                    Preset val ->
                        PresetSelection val
            )
            selection


finishInput : Posix -> Model -> Model
finishInput today model =
    let
        allowTime =
            case model.datePickerType of
                DatePicker ->
                    False

                DateRangePicker ->
                    True

        parseOutput =
            parseDateTime
                (List.map (\p -> presetToDisplayString p model.languageConfig) model.presets)
                model.languageConfig.dateFormatLanguage
                allowTime
                model.inputText
    in
    case parseOutput of
        Ok value ->
            let
                ( selection, format ) =
                    convertInput value model
            in
            { model
                | selection = selection
                , inputText = prettyFormatSelection selection model.languageConfig format
                , visibleCalendarRange = getVisibleRangeFromSelection selection model.calendarType today
                , displayFormat = format
            }

        Err _ ->
            { model | inputText = prettyFormatSelection model.selection model.languageConfig model.displayFormat }


finalizeSelection : Model -> InternalSelection -> InternalSelection
finalizeSelection model selection =
    case selection of
        SingleSelection posix ->
            SingleSelection <| getStartOfDay posix

        RangeSelection posixRange ->
            case model.datePickerType of
                DatePicker ->
                    SingleSelection <| getStartOfDay posixRange.start

                DateRangePicker ->
                    RangeSelection { start = getStartOfDay posixRange.start, end = getEndOfDay posixRange.end }

        Unselected ->
            selection

        Selecting _ ->
            selection

        PresetSelection _ ->
            selection


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
            , inputText = prettyFormatSelection selection model.languageConfig model.displayFormat
            , keyboardSelectedPreset = Nothing
        }


onKeyDown : Model -> Posix -> Key -> ( Model, Cmd Msg )
onKeyDown model today key =
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
                case model.datePickerType of
                    DateRangePicker ->
                        R2.withNoCmd { model | isShiftDown = True }

                    DatePicker ->
                        R2.withNoCmd model

        Escape ->
            if model.isPresetMenuOpen then
                R2.withNoCmd { model | isPresetMenuOpen = False, keyboardSelectedPreset = Nothing }

            else
                R2.withNoCmd { model | isOpen = False, uiButton = Nothing, uiElement = Nothing }

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
createSelectListWithLast allPresets =
    Maybe.map SelectList.selectLast (SelectList.fromList allPresets)


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
        SingleSelection _ ->
            { start = changedValue, end = changedValue }

        RangeSelection posixRange ->
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
                        |> RangeSelection
            in
            R2.withNoCmd
                { model
                    | isShiftDown = False
                    , terminationCounter = 10
                    , selection = selection
                    , inputText = prettyFormatSelection selection model.languageConfig model.displayFormat
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
    R2.withNoCmd
        { model
            | visibleCalendarRange =
                Just <|
                    calculateNewCalendarRange model intervalChange currentVisibleRange
        }


calculateNewCalendarRange : Model -> Int -> PosixRange -> PosixRange
calculateNewCalendarRange model intervalChange currentVisibleRange =
    let
        updateWithIntervalFunc intervalFunc range =
            { start = intervalFunc intervalChange utc range.start
            , end = intervalFunc intervalChange utc range.end
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
            calcRange adjustedToday model

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
                , mouseEvent
                , Html.Events.onMouseLeave <| SetMouseOutside False
                , Html.Events.onMouseEnter <| SetMouseOutside True
                ]
                []
            , div
                (List.append
                    [ Attrs.class "body"
                    , Attrs.id "elm-fancy--daterangepicker--wrapper"
                    , mouseEvent
                    ]
                    (calendarPositioning model.uiButton model.uiElement)
                )
                [ topBar model visibleRange adjustedToday zone
                , leftSelector visibleRange model zone
                , rightSelector visibleRange model zone
                , calendarView model today visibleRange zone
                , bottomBar model today
                ]
            ]

    else
        text ""


presetsDisplay : Model -> Posix -> Html Msg
presetsDisplay model today =
    if List.isEmpty model.presets || model.hidePresets then
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


bottomBar : Model -> Posix -> Html Msg
bottomBar model today =
    div [ Attrs.class "bottom-bar" ]
        [ button
            [ Attrs.id "elm-fancy--daterangepicker--done"
            , Attrs.class "done"
            , Html.Events.onClick <| Close today
            ]
            [ text model.languageConfig.done ]
        , button [ Attrs.class "reset", Html.Events.onClick Reset ]
            [ text model.languageConfig.reset ]
        ]


leftSelector : PosixRange -> Model -> Zone -> Html Msg
leftSelector =
    mkSelector -1 .end "prev-range-selector" "❮"


rightSelector : PosixRange -> Model -> Zone -> Html Msg
rightSelector =
    mkSelector 1 .start "next-range-selector" "❯"


mkSelector : Int -> (PosixRange -> Posix) -> String -> String -> PosixRange -> Model -> Zone -> Html Msg
mkSelector moveInterval partOfRange class textContent visibleRange model zone =
    let
        newRange =
            calculateNewCalendarRange model moveInterval visibleRange

        attrs =
            if posixIsOutOfAllowedRange (partOfRange newRange) model zone then
                [ Attrs.class "disabled" ]

            else
                [ onClick <| SetVisibleRange newRange ]
    in
    div
        ([ Attrs.class class ] ++ attrs)
        [ div [] [ text textContent ] ]


clockButton : Model -> Html Msg
clockButton model =
    let
        class =
            case model.displayFormat of
                DateFormat ->
                    Attrs.class "clock-unselected"

                DateTimeFormat ->
                    Attrs.class "clock-selected"
    in
    div
        [ class
        , Html.Events.onClick ToggleFormat
        , Attrs.title model.languageConfig.includeTimeTitle
        ]
        [ text "🕒" ]


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

        clock =
            case model.datePickerType of
                DatePicker ->
                    div [] []

                DateRangePicker ->
                    clockButton model

        selection =
            { start = getFirstDayOfYear utc visibleRange.start, end = getLastDayOfYear utc visibleRange.start }
                |> createSelectionInRange model zone
                |> RangeSelection
    in
    div [ Attrs.class class ]
        [ fullCalendarSelector
        , presetsDisplay model today
        , calendarInput model today
        , clock
        ]


createSelectionInRange : Model -> Zone -> PosixRange -> PosixRange
createSelectionInRange model zone posixRange =
    let
        startRange =
            dateToPosixRange model.availableForSelectionStart zone

        endRange =
            dateToPosixRange model.availableForSelectionEnd zone

        updatedSelectionStart =
            if posixToMillis posixRange.start < posixToMillis startRange.start then
                startRange.start

            else
                posixRange.start

        updatedSelectionEnd =
            if posixToMillis posixRange.end > posixToMillis endRange.end then
                endRange.end

            else
                posixRange.end
    in
    { start = updatedSelectionStart, end = updatedSelectionEnd }


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
            , Attrs.placeholder <| inputPlaceHolder model
            , Attrs.value model.inputText
            ]
            []
        ]


inputPlaceHolder : Model -> String
inputPlaceHolder model =
    case model.datePickerType of
        DateRangePicker ->
            model.languageConfig.dateRangePickerInputPlaceholder

        DatePicker ->
            model.languageConfig.datePickerInputPlaceholder


convertInput : Input -> Model -> ( InternalSelection, Format )
convertInput input model =
    case input of
        SingleInput inputDate ->
            ( convertInputDate inputDate, DateFormat )

        RangeInput start end ->
            combineInputToRange start end

        CustomDate selectedCustomDate ->
            let
                selectedPreset =
                    List.filter (\p -> presetToDisplayString p model.languageConfig == selectedCustomDate) model.presets
            in
            case List.head selectedPreset of
                Just preset ->
                    ( PresetSelection preset, DateFormat )

                Nothing ->
                    ( Unselected, DateFormat )


combineInputToRange : InputDate -> InputDate -> ( InternalSelection, Format )
combineInputToRange start end =
    let
        startSelection =
            convertInputDate start

        endSelection =
            convertInputDate end
    in
    case ( startSelection, endSelection ) of
        ( SingleSelection startPosix, SingleSelection endPosix ) ->
            ( RangeSelection { start = startPosix, end = endPosix }, DateTimeFormat )

        ( SingleSelection startPosix, RangeSelection endPosixRange ) ->
            ( RangeSelection { start = startPosix, end = endPosixRange.end }, DateTimeFormat )

        ( RangeSelection startPosixRange, SingleSelection endPosix ) ->
            ( RangeSelection { start = startPosixRange.start, end = endPosix }, DateTimeFormat )

        ( RangeSelection startPosixRange, RangeSelection endPosixRange ) ->
            ( RangeSelection { start = startPosixRange.start, end = endPosixRange.end }, DateFormat )

        _ ->
            ( Unselected, DateFormat )


convertInputDate : InputDate -> InternalSelection
convertInputDate inputDate =
    case inputDate of
        JustYear year ->
            RangeSelection <| yearToPosixRange year utc

        JustYearAndMonth yearAndMonth ->
            RangeSelection <| yearAndMonthToPosixRange yearAndMonth utc

        FullDate dateParts ->
            SingleSelection <| datePartsToPosix dateParts utc

        FullDateTime dateTimeParts ->
            SingleSelection <| dateTimePartsToPosix dateTimeParts utc


calendarView : Model -> Posix -> PosixRange -> Zone -> Html Msg
calendarView model =
    case model.calendarType of
        FullCalendar ->
            yearCalendarView model

        ThreeMonths ->
            monthlyCalendarView model "monthly-small" 2

        TwoMonths ->
            monthlyCalendarView model "monthly-large" 1

        OneMonth ->
            monthlyCalendarView model "monthly-large" 0


yearCalendarView : Model -> Posix -> PosixRange -> Zone -> Html Msg
yearCalendarView model today visibleRange zone =
    let
        quarter name startMonth endMonth =
            let
                posixRangeForQuarter =
                    posixRangeForMonths startMonth endMonth (Time.toYear utc visibleRange.start) utc

                isOutOfRange =
                    posixIsOutOfAllowedRange posixRangeForQuarter.start model zone
                        || posixIsOutOfAllowedRange posixRangeForQuarter.end model zone

                attrs =
                    if isOutOfRange then
                        [ Attrs.class "disabled" ]

                    else
                        [ posixRangeForQuarter
                            |> RangeSelection
                            |> SetSelection
                            |> Html.Events.onClick
                        , Attrs.class "selection-hover"
                        ]
            in
            div
                attrs
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
                List.map (\m -> monthCalendarView m today model zone) (getMonthsFromRange 0 11 visibleRange getFirstDayOfYear)
            ]
        ]


monthlyCalendarView : Model -> String -> Int -> Posix -> PosixRange -> Zone -> Html Msg
monthlyCalendarView model monthClass endInterval today visibleRange zone =
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "month-calendar" ]
        [ table []
            [ tbody [ Attrs.class monthClass ] <|
                List.map (\m -> monthCalendarView m today model zone) (getMonthsFromRange 0 endInterval visibleRange getFirstDayOfMonthStartOfDay)
            ]
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


monthCalendarView : Posix -> Posix -> Model -> Zone -> Html Msg
monthCalendarView currentMonth today model zone =
    let
        selection =
            { start = getFirstDayOfMonthStartOfDay utc currentMonth, end = getLastDayOfMonthEndOfDay utc currentMonth }
                |> createSelectionInRange model zone
                |> RangeSelection

        wholeMonthIsOutOfRange =
            posixIsOutOfAllowedRange (getFirstDayOfMonthStartOfDay utc currentMonth) model zone
                && posixIsOutOfAllowedRange (getLastDayOfMonthEndOfDay utc currentMonth) model zone

        attrs =
            if wholeMonthIsOutOfRange then
                [ Attrs.class "disabled", Attrs.class "month--header" ]

            else
                [ Attrs.class "month--header", onClick <| SetSelection selection ]
    in
    td []
        [ table []
            [ thead attrs
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

        ( hoverAttr, content, setDate ) =
            if contentIsInCorrectMonth then
                if posixIsOutOfAllowedRange currentDay model zone then
                    ( Attrs.class "", [ text <| String.fromInt <| Time.toDay utc currentDay ], Attrs.class "" )

                else
                    ( Html.Events.onMouseOver <| OnHoverOverDay currentDay
                    , [ text <| String.fromInt <| Time.toDay utc currentDay ]
                    , setDateAttr
                    )

            else
                ( Attrs.class "", [], Attrs.class "" )

        setDateAttr =
            if model.isShiftDown || model.isMouseDown then
                Just currentDay |> EndSelection |> onClickNoDefault

            else
                StartSelection currentDay |> DateRangePicker.Helper.mouseDownNoDefault

        isSameDayOfSelection posixFromSelection =
            contentIsInCorrectMonth && (Maybe.withDefault False <| Maybe.map (\p -> isSameDay p currentDay) posixFromSelection)

        ( selectionStart, selectionEnd, isInSelectionRange ) =
            selectionPoints currentDay model today zone

        classList =
            Attrs.classList
                [ ( "day", True )
                , ( "selected-range", contentIsInCorrectMonth && isInSelectionRange )
                , ( "border-selection", isSameDayOfSelection selectionStart || isSameDayOfSelection selectionEnd )
                , ( "today", isSameDay currentDay today )
                , ( "disabled", posixIsOutOfAllowedRange currentDay model zone )
                , ( "wrong-month", not contentIsInCorrectMonth )
                ]
    in
    td [ classList, setDate, hoverAttr ] [ div [] content ]


calendarPositioning : Maybe Element -> Maybe Element -> List (Attribute msg)
calendarPositioning buttonElement calendarElement =
    case ( buttonElement, calendarElement ) of
        ( Just button, Just calendar ) ->
            [ calculateYPosition button calendar
            , calculateXPosition button calendar
            ]

        _ ->
            [ Attrs.style "left" "-9999px" ]


addPx : String -> String
addPx str =
    str ++ "px"


calculateYPosition : Element -> Element -> Attribute msg
calculateYPosition button calendar =
    let
        ( yNum, yName ) =
            if button.element.y < (button.scene.height / 2) then
                ( additionalCalcForTop -button.element.height, "top" )

            else
                ( additionalCalcForBottom 0, "bottom" )

        additionalCalcForBottom num =
            if button.element.y > calendar.element.height then
                num

            else
                -button.element.y

        additionalCalcForTop num =
            if (button.scene.height - button.element.y) > calendar.element.height then
                num

            else
                -(button.element.height + button.element.y)
    in
    Attrs.style yName
        (yNum
            |> String.fromFloat
            |> addPx
        )


calculateXPosition : Element -> Element -> Attribute msg
calculateXPosition button calendar =
    let
        ( xNum, xName ) =
            if button.element.x > (button.scene.width / 2) then
                additionalCalcForRight 0 "right"

            else
                ( additionalCalcForLeft 0, "left" )

        additionalCalcForLeft num =
            if button.element.x > calendar.element.width then
                -button.element.x

            else
                num

        additionalCalcForRight num curPosName =
            if (button.scene.width - button.element.x) > (calendar.element.width + 15) then
                ( -((button.scene.width - button.element.x) - calendar.element.width), "left" )

            else
                ( num, curPosName )
    in
    Attrs.style xName
        (xNum
            |> String.fromFloat
            |> addPx
        )


dateToPosixRange : Date -> Zone -> PosixRange
dateToPosixRange d zone =
    datePartsToPosixRange
        { year = Date.year d
        , month = Date.monthToNumber <| Date.month d
        , day = Date.day d
        }
        zone


posixIsOutOfAllowedRange : Posix -> Model -> Zone -> Bool
posixIsOutOfAllowedRange posix model zone =
    let
        start =
            dateToPosixRange model.availableForSelectionStart zone

        end =
            dateToPosixRange model.availableForSelectionEnd zone
    in
    posixToMillis posix < posixToMillis start.start || posixToMillis posix > posixToMillis end.end


normalizeSelectingRange : PosixRange -> PosixRange
normalizeSelectingRange posixRange =
    if posixToMillis posixRange.start > posixToMillis posixRange.end then
        { start = posixRange.end, end = posixRange.start }

    else
        posixRange


selectionPoints : Posix -> Model -> Posix -> Zone -> ( Maybe Posix, Maybe Posix, Bool )
selectionPoints comparisonPosix { selection } today localZone =
    let
        posixInMillis =
            posixToMillis comparisonPosix

        compareRange range =
            posixToMillis range.start <= posixInMillis && posixInMillis <= posixToMillis range.end
    in
    case selection of
        SingleSelection posix ->
            ( Just posix, Just posix, False )

        RangeSelection posixRange ->
            ( Just posixRange.start, Just posixRange.end, compareRange posixRange )

        Unselected ->
            ( Nothing, Nothing, False )

        Selecting posixRange ->
            let
                normalized =
                    normalizeSelectingRange posixRange
            in
            ( Just normalized.start, Just normalized.end, compareRange normalized )

        PresetSelection presetType ->
            let
                posixRange =
                    presetToPosixRange presetType today localZone
            in
            ( Just posixRange.start, Just posixRange.end, compareRange posixRange )


isSameDay : Posix -> Posix -> Bool
isSameDay posix1 posix2 =
    let
        civel1 =
            posixToCivil posix1

        civel2 =
            posixToCivil posix2
    in
    civel1.day == civel2.day && civel1.month == civel2.month && civel1.year == civel2.year


calcRange : Posix -> Model -> PosixRange
calcRange today model =
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
        SingleSelection posix ->
            Just { start = getStartOfDay posix, end = getEndOfDay posix }

        RangeSelection posixRange ->
            convertToRange posixRange.start calendarType
                |> Just

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Just <| presetToPosixRange presetType today utc


prettyFormatSelection : InternalSelection -> LanguageConfig -> Format -> String
prettyFormatSelection selection language format =
    case selection of
        SingleSelection posix ->
            singleFormatter language format posix

        RangeSelection posixRange ->
            if isSameDay posixRange.start posixRange.end && format /= DateTimeFormat then
                singleFormatter language format posixRange.start

            else
                fullFormatter language format posixRange.start posixRange.end

        Unselected ->
            ""

        Selecting _ ->
            ""

        PresetSelection presetType ->
            presetToDisplayString presetType language


singleFormatter : LanguageConfig -> Format -> Posix -> String
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
        utc


monthFormatter : LanguageConfig -> Zone -> Posix -> String
monthFormatter language =
    DateFormat.formatWithLanguage language.dateFormatLanguage
        [ DateFormat.monthNameFull
        ]


fullFormatter : LanguageConfig -> Format -> Posix -> Posix -> String
fullFormatter language format start end =
    singleFormatter language format start
        ++ " to "
        ++ singleFormatter language format end



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
    { start = getStartOfDay <| getFirstDayOfYear zone posix
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


{-| A helper function to get the end of the day in time given a date
-}
getEndOfDay : Posix -> Posix
getEndOfDay posix =
    let
        dateRecord =
            posixToCivil posix

        updatedDateRecord =
            { dateRecord
                | hour = 23
                , minute = 59
                , second = 59
                , millis = 999
            }
    in
    civilToPosix updatedDateRecord


{-| A helper function to get the start of the day in time given a date
-}
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
    civilToPosix updatedDateRecord


isOpen : Model -> Bool
isOpen model =
    model.isOpen


setOpen : Model -> Bool -> Model
setOpen model openState =
    { model | isOpen = openState }


hasUtcSelectionChanged : Model -> Maybe Selection -> Zone -> Bool
hasUtcSelectionChanged model comparisonSelection localZone =
    checkForChange (utcSelection localZone) model comparisonSelection


hasLocalSelectionChanged : Model -> Maybe Selection -> Bool
hasLocalSelectionChanged =
    checkForChange localSelection


hasUtcRangeChanged : Model -> Maybe PosixRange -> Zone -> Posix -> Bool
hasUtcRangeChanged model comparisonRange localZone today =
    checkForChange (utcSelectionRange localZone today) model comparisonRange


hasLocalRangeChanged : Model -> Maybe PosixRange -> Posix -> Bool
hasLocalRangeChanged model comparisonRange today =
    checkForChange (localSelectionRange today) model comparisonRange


checkForChange : (Model -> Maybe a) -> Model -> Maybe a -> Bool
checkForChange checkFunc model elementForComparison =
    if model.isOpen then
        False

    else
        checkFunc model /= elementForComparison


{-| A helper function to change the calendar type on an existing model
-}
setCalendarType : CalendarType -> Model -> Model
setCalendarType calendarType model =
    { model | calendarType = calendarType }


localSelection : Model -> Maybe Selection
localSelection model =
    case model.selection of
        SingleSelection pos ->
            Just <| Single pos

        RangeSelection range ->
            Just <| Range range

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Just <| Preset presetType


utcSelection : Zone -> Model -> Maybe Selection
utcSelection zone model =
    case model.selection of
        SingleSelection pos ->
            Single (adjustMilliseconds zone pos)
                |> Just

        RangeSelection range ->
            Just <| Range <| convertRangeToUtc zone range

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Just <| Preset presetType


convertRangeToUtc : Zone -> PosixRange -> PosixRange
convertRangeToUtc zone { start, end } =
    { start = adjustMilliseconds zone start, end = adjustMilliseconds zone end }


utcSelectionRange : Zone -> Posix -> Model -> Maybe PosixRange
utcSelectionRange zone today model =
    case model.selection of
        SingleSelection pos ->
            { start = adjustMilliseconds zone <| getStartOfDay pos, end = adjustMilliseconds zone <| getEndOfDay pos }
                |> Just

        RangeSelection range ->
            Just <| convertRangeToUtc zone range

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Just <| presetToUtcPosixRange presetType today zone


localSelectionRange : Posix -> Model -> Maybe PosixRange
localSelectionRange today model =
    case model.selection of
        SingleSelection pos ->
            Just <| { start = getStartOfDay pos, end = getEndOfDay pos }

        RangeSelection range ->
            Just range

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Just <| presetToLocalPosixRange presetType today


utcSelectionSingle : Zone -> Posix -> Model -> Maybe Posix
utcSelectionSingle zone today model =
    Maybe.map .start (utcSelectionRange zone today model)


localSelectionSingle : Posix -> Model -> Maybe Posix
localSelectionSingle today model =
    Maybe.map .start (localSelectionRange today model)


presets : Model -> List PresetType
presets model =
    model.presets


languageConfig : Model -> LanguageConfig
languageConfig model =
    model.languageConfig


updateModelWithConfig : Model -> Config -> Model
updateModelWithConfig model config =
    { model
        | availableForSelectionStart = config.availableForSelectionStart
        , availableForSelectionEnd = config.availableForSelectionEnd
        , presets = config.presets
        , calendarType = config.calendarType
        , isOpen = config.isOpen
        , datePickerType = config.datePickerType
        , hidePresets = config.hidePresets
        , selection = selectionToInternalSelection config.defaultSelection
    }


setSelection : Model -> Maybe Selection -> Model
setSelection model selection =
    { model | selection = selectionToInternalSelection selection }



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


{-| A helper function to get the posix range for a given preset in utc time
-}
presetToUtcPosixRange : PresetType -> Posix -> Zone -> PosixRange
presetToUtcPosixRange presetType today zone =
    convertRangeToUtc zone <| presetToPosixRange presetType today zone


{-| A helper function to get the posix range for a given preset in local time
-}
presetToLocalPosixRange : PresetType -> Posix -> PosixRange
presetToLocalPosixRange presetType today =
    presetToPosixRange presetType today utc


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


calendarIcon : Html msg
calendarIcon =
    svg [ Svg.width "30", Svg.height "30", Svg.viewBox "0 0 30 30" ]
        [ g [ Svg.stroke "none", Svg.strokeWidth "1", Svg.fill "none", Svg.fillRule "evenodd", Svg.strokeLinecap "round", Svg.strokeLinejoin "round" ]
            [ g [ Svg.transform "translate(6.000000, 5.000000)", Svg.stroke "currentColor", Svg.strokeWidth "2" ]
                [ Svg.rect [ Svg.x "0", Svg.y "2", Svg.width "18", Svg.height "18", Svg.rx "2" ]
                    []
                , path [ Svg.d "M13,0 L13,4" ]
                    []
                , path [ Svg.d "M5,0 L5,4" ]
                    []
                , path [ Svg.d "M0,8 L18,8" ]
                    []
                ]
            ]
        ]
