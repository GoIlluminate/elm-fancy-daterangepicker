module DateRangePicker exposing
    ( Msg, DatePicker, subscriptions, view, update
    , init, open, defaultOpener
    , Selection(..), Format(..), PosixRange, localSelection, localSelectionRange, localSelectionSingle, utcSelection, utcSelectionRange, utcSelectionSingle
    , Config, LanguageConfig, englishLanguageConfig, DateSelectionType(..), PresetType(..), Interval(..), CustomPreset, CalendarType(..), defaultConfig, initWithOptions, updateModelWithConfig
    , setCalendarType, isOpen, presets, setOpen, setSelection, languageConfig, selectPreset, displayFormat
    , presetToDisplayString, getEndOfDay, getStartOfDay, hasLocalRangeChanged, hasLocalSelectionChanged, hasUtcRangeChanged, hasUtcSelectionChanged, presetToLocalPosixRange, presetToUtcPosixRange, displaySelection, displayUtcSelection
    )

{-| A customizable date picker component.


# Basics

@docs Msg, DatePicker, subscriptions, view, update

@docs init, open, defaultOpener


# Selection

@docs Selection, Format, PosixRange, localSelection, localSelectionRange, localSelectionSingle, utcSelection, utcSelectionRange, utcSelectionSingle


# Settings

@docs Config, LanguageConfig, englishLanguageConfig, DateSelectionType, PresetType, Interval, CustomPreset, CalendarType, defaultConfig, initWithOptions, updateModelWithConfig


# Model Helpers

@docs setCalendarType, isOpen, presets, setOpen, setSelection, languageConfig, selectPreset, displayFormat


# Helpers

@docs presetToDisplayString, getEndOfDay, getStartOfDay, hasLocalRangeChanged, hasLocalSelectionChanged, hasUtcRangeChanged, hasUtcSelectionChanged, presetToLocalPosixRange, presetToUtcPosixRange, selectPreset, displaySelection, displayUtcSelection

-}

import Browser.Dom as Dom exposing (Element, Error, Viewport, getElement)
import Browser.Events
import Date exposing (Date)
import DateFormat
import DateFormat.Language as DateFormat
import DateRangePicker.DateRecordParser exposing (DateParts, Input(..), InputDate(..), YearAndMonth, datePartsToPosix, dateTimePartsToPosix, parseDateTime, yearAndMonthToPosix, yearToPosix)
import DateRangePicker.Helper exposing (onClickNoDefault)
import Derberos.Date.Calendar exposing (getCurrentMonthDatesFullWeeks, getFirstDayOfMonth, getFirstDayOfYear, getLastDayOfMonth, getLastDayOfYear)
import Derberos.Date.Core as DateCore exposing (civilToPosix, posixToCivil)
import Derberos.Date.Delta exposing (addDays, addMonths, addYears, nextWeekdayFromTime, prevWeekdayFromTime)
import Derberos.Date.Utils exposing (getNextMonth, getPrevMonth, getWeekday, monthToNumber, monthToNumber1, numberOfDaysInMonth, numberToMonth)
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
import Time exposing (Month(..), Posix, Weekday(..), Zone, posixToMillis)
import Time.Extra as TimeExtra exposing (Parts, partsToPosix, posixToParts)


{-| An opaque type representing messages that are passed inside the DatePicker.
-}
type Msg
    = DoNothing
    | Open String
    | Close Posix
    | SetVisibleRange PartsRange
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
    | OnWindowsResize Int Int
    | GotViewPort (Result Error Viewport)


type MousePosition
    = Inside
    | OutsideTop
    | OutsideLeft
    | OutsideRight
    | OutsideBottom


{-| The type of that represents a range of two posix times.
-}
type alias PosixRange =
    { start : Posix
    , end : Posix
    }


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



{- Stored as UTC -}


type InternalSelection
    = SingleSelection Posix
    | RangeSelection PosixRange
    | Unselected
    | Selecting PosixRange
    | PresetSelection PresetType
    | BeforeSelection Posix
    | AfterSelection Posix


{-| The type which represents what the current selection is in the datepicker.

If you select a preset you can use @presetToPosixRange to get the appropriate posix range for the selection.

-}
type Selection
    = Single Posix
    | Range PosixRange
    | Preset PresetType
    | Before Posix
    | After Posix


{-| The type which specifies what size calendar you want to display
-}
type CalendarType
    = FullCalendar
    | ThreeMonths
    | TwoMonths
    | OneMonth


type DateSelectionType
    = DateRangeSelection
    | DateSelection


{-| The type which specifies if a user had specified a time in the input box as well as the selected date.
-}
type Format
    = DateFormat
    | DateTimeFormat


type alias WindowSize =
    { width : Float
    , height : Float
    }


type alias PartsRange =
    { start : Parts
    , end : Parts
    }


{-| A record which represents the main datepicker model
Selection stored as UTC
Presets Are currently stored as Local
HoveredDate is Utc
-}
type alias Model =
    { selection : InternalSelection
    , availableForSelectionStart : Posix
    , availableForSelectionEnd : Posix
    , visibleCalendarRange : Maybe PartsRange
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
    , dateSelectionType : DateSelectionType
    , hidePresets : Bool
    , windowSize : WindowSize
    }


{-| A record which represents the main datepicker model
-}
type DatePicker
    = DatePicker Model


{-| Initialize the datepicker with the default settings
-}
init : DatePicker
init =
    DatePicker <|
        { selection = Unselected
        , availableForSelectionStart = civilToPosix <| DateCore.newDateRecord 1900 1 1 0 0 0 0 Time.utc
        , availableForSelectionEnd = civilToPosix <| DateCore.newDateRecord 2100 12 31 23 59 59 0 Time.utc
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
        , dateSelectionType = DateRangeSelection
        , hidePresets = False
        , windowSize = WindowSize 0 0
        }


{-| A record which specifies config options which can be set when initializes the datepicker
-}
type alias Config =
    { availableForSelectionStart : Posix
    , availableForSelectionEnd : Posix
    , presets : List PresetType
    , calendarType : CalendarType
    , isOpen : Bool
    , languageConfig : LanguageConfig
    , dateSelectionType : DateSelectionType
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
    , beforeThisDate : String
    , afterThisDate : String
    , am : String
    , pm : String
    , to : String
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
    , beforeThisDate = "And Before"
    , afterThisDate = "And After"
    , am = "am"
    , pm = "pm"
    , to = "to"
    }


{-| Initialize the datepicker with the custom settings
-}
initWithOptions : Config -> DatePicker
initWithOptions config =
    updateModelWithConfig init config


{-| A default config which can be combined with @initWithOptions so that you only need to specify the fields which you want to customize
-}
defaultConfig : Config
defaultConfig =
    { availableForSelectionStart = civilToPosix <| DateCore.newDateRecord 1900 1 1 0 0 0 0 Time.utc
    , availableForSelectionEnd = civilToPosix <| DateCore.newDateRecord 2100 12 31 23 59 59 0 Time.utc
    , presets = []
    , calendarType = FullCalendar
    , isOpen = False
    , languageConfig = englishLanguageConfig
    , dateSelectionType = DateRangeSelection
    , hidePresets = False
    , defaultSelection = Nothing
    }


{-| A helper attribute which allows you to open the datepicker using any html element.

    The element you place this attribute on has to have an id so that the datepicker can dynamically be placed.

    Html.map DatePickerMsgs <| button [Html.Attribute.id openerId, open openerId ] [ text "Open Me!" ]

You will need to call convert the message to the appropriate type via Html.map

-}
open : String -> Attribute Msg
open openerId =
    Html.Events.onClick (Open openerId)


{-| A pre-made opener that can be used to open the datepicker instead of @open.

    It will need an id so that the datepicker can dynamically be placed.

    Html.map DatePickerMsgs <| defaultOpener model.datePicker openerId

You will need to call convert the message to the appropriate type via Html.map

-}
defaultOpener : Zone -> DatePicker -> String -> Html Msg
defaultOpener zone (DatePicker model) openerId =
    let
        selectionValue =
            prettyFormatSelection zone model.selection model.languageConfig model.displayFormat

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

    Pass in Today as utc

-}
subscriptions : DatePicker -> Posix -> Zone -> Sub Msg
subscriptions (DatePicker model) today zone =
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
            [ Keyboard.downs (KeyDown today) ]

        mouseSubs =
            if model.isMouseDown then
                [ Browser.Events.onMouseUp (EndSelection model.currentlyHoveredDate |> Json.succeed)
                , Browser.Events.onVisibilityChange (EndSelection model.currentlyHoveredDate |> always)
                , Time.every 1250 (always <| CheckToMoveToNextVisibleRange today)
                ]

            else
                []

        window =
            Browser.Events.onResize OnWindowsResize

        closeSub =
            if model.isMouseOutside && not model.isMouseDown then
                [ Browser.Events.onClick (Json.succeed <| Close today) ]

            else
                []
    in
    if model.isOpen then
        List.concat [ shiftSubs, mouseSubs, keyDowns, closeSub ] |> Sub.batch

    else
        window


{-| The update for the datepicker. You will need to integrate this into your own update.

    DatePickerMsgs msg_ ->
        let
            ( newDateRangePicker, dateRangePickerCmd ) =
                DateRangePicker.update msg_ model.datePicker
        in
        ( { model | datePicker = newDateRangePicker }, Cmd.map DatePickerMsgs dateRangePickerCmd )

-}
update : Zone -> Msg -> DatePicker -> ( DatePicker, Cmd Msg )
update zone msg (DatePicker model) =
    let
        ( model_, cmd ) =
            innerUpdate zone msg model
    in
    ( DatePicker model_, cmd )


{-| The view for the datepicker. You will have to pass in the current time as well as the local zone and the datepicker model.

    DateRangePicker.view currentTime localZone datePicker


    Pass in today as utc.

-}
view : Posix -> Zone -> DatePicker -> Html Msg
view today zone (DatePicker model) =
    let
        -- Adjust today with the timezone and then every other view below uses utc which does not adjust the time when manipulating it
        todayParts =
            posixToParts zone today

        visibleRangeParts =
            Maybe.withDefault (calcVisibleRange todayParts model) model.visibleCalendarRange

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
                    (calendarPositioning model.uiButton model.uiElement model.windowSize)
                )
                [ topBar model visibleRangeParts todayParts zone
                , leftSelector visibleRangeParts model zone
                , rightSelector visibleRangeParts model zone
                , calendarView model todayParts visibleRangeParts zone
                , bottomBar model zone todayParts
                ]
            ]

    else
        text ""


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


{-| A helper function to set a preset outside of the default preset ui
-}
selectPreset : PresetType -> Posix -> Zone -> DatePicker -> ( DatePicker, Cmd Msg )
selectPreset presetType today zone (DatePicker model) =
    let
        ( model_, cmd ) =
            selectPresetInternal presetType today zone model
    in
    ( DatePicker model_, cmd )


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


{-| Checks if the datepicker is open
-}
isOpen : DatePicker -> Bool
isOpen (DatePicker model) =
    model.isOpen


{-| Gets the current selection format
-}
displayFormat : DatePicker -> Format
displayFormat (DatePicker model) =
    model.displayFormat


{-| Set whether or not the datepicker is open. Usually you should use @open or @defaultOpener to manage this.
-}
setOpen : DatePicker -> Bool -> DatePicker
setOpen (DatePicker model) openState =
    DatePicker { model | isOpen = openState }


{-| Check if the selection has changed.
-}
hasUtcSelectionChanged : DatePicker -> Maybe Selection -> Bool
hasUtcSelectionChanged model comparisonSelection =
    checkForChange utcSelection model comparisonSelection


{-| Check if the selection has changed.
-}
hasLocalSelectionChanged : DatePicker -> Maybe Selection -> Bool
hasLocalSelectionChanged =
    checkForChange localSelection


{-| Check if the selection has changed.
-}
hasUtcRangeChanged : DatePicker -> Maybe PosixRange -> Posix -> Zone -> Bool
hasUtcRangeChanged model comparisonRange today zone =
    checkForChange (utcSelectionRange zone today) model comparisonRange


{-| Check if the selection has changed.
-}
hasLocalRangeChanged : DatePicker -> Maybe PosixRange -> Posix -> Zone -> Bool
hasLocalRangeChanged model comparisonRange today zone =
    checkForChange (localSelectionRange zone today) model comparisonRange


{-| A helper function to change the calendar type on an existing model. Usually you should use @initWithOptions and configure this at initialization.
-}
setCalendarType : CalendarType -> DatePicker -> DatePicker
setCalendarType calendarType (DatePicker model) =
    DatePicker { model | calendarType = calendarType }


{-| Get the current selection in local time.
-}
localSelection : DatePicker -> Maybe Selection
localSelection (DatePicker model) =
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

        BeforeSelection pos ->
            Just <| Before pos

        AfterSelection pos ->
            Just <| After pos


{-| Get the current selection in utc time.
-}
utcSelection : DatePicker -> Maybe Selection
utcSelection (DatePicker model) =
    case model.selection of
        SingleSelection pos ->
            Single pos
                |> Just

        RangeSelection range ->
            Just <| Range range

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Just <| Preset presetType

        BeforeSelection pos ->
            Just <| Before pos

        AfterSelection pos ->
            Just <| After pos


{-| A convenience function to get the current selection as a posix range in utc time.

Pass in today as utc
Pass in Zone to convert the presets to utc

-}
utcSelectionRange : Zone -> Posix -> DatePicker -> Maybe PosixRange
utcSelectionRange zone today (DatePicker model) =
    case model.selection of
        SingleSelection pos ->
            Just <| convertSingleIntoRange pos

        RangeSelection range ->
            Just range

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Just <| presetToUtcPosixRange presetType today zone

        BeforeSelection pos ->
            Just <| convertSingleIntoRange pos

        AfterSelection pos ->
            Just <| convertSingleIntoRange pos


{-| A convenience function to get the current selection as a posix range in local time.
-}
localSelectionRange : Zone -> Posix -> DatePicker -> Maybe PosixRange
localSelectionRange zone today (DatePicker model) =
    case model.selection of
        SingleSelection pos ->
            Just <| convertSingleIntoRange pos

        RangeSelection range ->
            Just range

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            Just <| presetToLocalPosixRange presetType today

        BeforeSelection pos ->
            Just <| convertSingleIntoRange pos

        AfterSelection pos ->
            Just <| convertSingleIntoRange pos


{-| A convenience function to get the current selection as a single posix in utc time. This is particularly useful when you only allow for single date selection.
-}
utcSelectionSingle : Zone -> Posix -> DatePicker -> Maybe Posix
utcSelectionSingle zone today model =
    Maybe.map .start (utcSelectionRange zone today model)


{-| A convenience function to get the current selection as a single posix in local time. This is particularly useful when you only allow for single date selection.
-}
localSelectionSingle : Zone -> Posix -> DatePicker -> Maybe Posix
localSelectionSingle zone today model =
    Maybe.map .start (localSelectionRange zone today model)


{-| Gets the current presets
-}
presets : DatePicker -> List PresetType
presets (DatePicker model) =
    model.presets


{-| Gets the current languageConfig
-}
languageConfig : DatePicker -> LanguageConfig
languageConfig (DatePicker model) =
    model.languageConfig


{-| Change the datePicker's current config. This will reset changed state.
-}
updateModelWithConfig : DatePicker -> Config -> DatePicker
updateModelWithConfig (DatePicker model) config =
    DatePicker
        { model
            | availableForSelectionStart = config.availableForSelectionStart
            , availableForSelectionEnd = config.availableForSelectionEnd
            , presets = config.presets
            , calendarType = config.calendarType
            , isOpen = config.isOpen
            , dateSelectionType = config.dateSelectionType
            , hidePresets = config.hidePresets
            , selection = selectionToInternalSelection config.defaultSelection
        }


{-| Sets the datepicker's selection outside of the ui. Normally the ui should have all the interactions you would want.
-}
setSelection : Zone -> Maybe Selection -> DatePicker -> DatePicker
setSelection zone selection (DatePicker model) =
    case selection of
        Just _ ->
            DatePicker
                { model
                    | selection = selectionToInternalSelection selection
                    , inputText = prettyFormatSelection zone (selectionToInternalSelection selection) model.languageConfig model.displayFormat
                }

        Nothing ->
            DatePicker
                { model
                    | selection = selectionToInternalSelection selection
                    , inputText = ""
                }


{-| A helper function to get the posix range for a given preset in utc time
-}
presetToUtcPosixRange : PresetType -> Posix -> Zone -> PosixRange
presetToUtcPosixRange presetType today zone =
    presetToPosixRange presetType today zone


{-| A helper function to get the posix range for a given preset in local time
-}
presetToLocalPosixRange : PresetType -> Posix -> PosixRange
presetToLocalPosixRange presetType today =
    --TODO
    presetToPosixRange presetType today Time.utc


{-| A helper function to display the selection in the same way that the datepicker does
-}
displaySelection : Zone -> DatePicker -> String
displaySelection zone (DatePicker model) =
    prettyFormatSelection zone model.selection model.languageConfig model.displayFormat


{-| A helper function to display the selection in the same way that the datepicker does. It is in utc time.
-}
displayUtcSelection : DatePicker -> String
displayUtcSelection datePicker =
    let
        selection =
            selectionToInternalSelection <| utcSelection datePicker

        format (DatePicker picker) =
            prettyFormatSelection Time.utc selection picker.languageConfig picker.displayFormat
    in
    format datePicker


innerUpdate : Zone -> Msg -> Model -> ( Model, Cmd Msg )
innerUpdate zone msg model =
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
                , Task.attempt GotViewPort <|
                    Dom.getViewport
                ]
                { model | isOpen = True }

        Close today ->
            R2.withNoCmd <| finishInput today zone { model | isOpen = False, uiButton = Nothing, uiElement = Nothing, isMouseOutside = False }

        SetVisibleRange visibleCalendarRange ->
            R2.withNoCmd
                { model
                    | visibleCalendarRange = Just visibleCalendarRange
                }

        SetSelection selection ->
            R2.withNoCmd
                { model
                    | selection = selection
                    , inputText = prettyFormatSelection zone selection model.languageConfig model.displayFormat
                }

        OnInputFinish today ->
            R2.withNoCmd <| finishInput today zone model

        OnInputChange newText ->
            R2.withNoCmd { model | inputText = newText }

        Reset ->
            R2.withNoCmd { model | inputText = "", selection = Unselected, visibleCalendarRange = Nothing }

        StartSelection posix ->
            case model.dateSelectionType of
                DateSelection ->
                    let
                        selection =
                            SingleSelection posix
                    in
                    R2.withNoCmd
                        { model
                            | selection = selection
                            , inputText = prettyFormatSelection zone selection model.languageConfig model.displayFormat
                        }

                DateRangeSelection ->
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
                                |> posixToParts zone
                                |> getEndOfDayParts
                                |> partsToPosix zone
                                |> createSelectingRange model
                                |> normalizeSelectingRange
                                |> RangeSelection
                    in
                    R2.withNoCmd
                        { model
                            | isMouseDown = False
                            , selection = selection
                            , inputText = prettyFormatSelection zone selection model.languageConfig model.displayFormat
                        }

                Nothing ->
                    R2.withNoCmd model

        KeyDown today rawKey ->
            onKey rawKey model (onKeyDown model today zone)

        KeyUp rawKey ->
            onKey rawKey
                model
                (\key ->
                    if key == Shift then
                        cancelShift zone model

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
            cancelShift zone model

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
                localToday =
                    posixToParts zone today

                visibleRange =
                    calcVisibleRange localToday model
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
            selectPresetInternal presetType today zone model

        ToggleFormat ->
            let
                updatedModel =
                    case model.displayFormat of
                        DateFormat ->
                            { model
                                | displayFormat = DateTimeFormat
                                , inputText = prettyFormatSelection zone model.selection model.languageConfig DateTimeFormat
                            }

                        DateTimeFormat ->
                            { model
                                | displayFormat = DateFormat
                                , inputText = prettyFormatSelection zone model.selection model.languageConfig DateFormat
                                , selection = model.selection
                            }
            in
            R2.withNoCmd updatedModel

        OnWindowsResize width height ->
            R2.withNoCmd { model | windowSize = WindowSize (toFloat width) (toFloat height) }

        GotViewPort (Result.Ok viewPort) ->
            R2.withNoCmd { model | windowSize = WindowSize viewPort.viewport.width viewPort.viewport.height }

        GotViewPort _ ->
            R2.withNoCmd model


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

                    Before pos ->
                        BeforeSelection pos

                    After pos ->
                        AfterSelection pos
            )
            selection


finishInput : Posix -> Zone -> Model -> Model
finishInput today zone model =
    let
        allowTime =
            case model.dateSelectionType of
                DateSelection ->
                    False

                DateRangeSelection ->
                    True

        parseConfig =
            { customDateInputs = List.map (\p -> presetToDisplayString p model.languageConfig) model.presets
            , language =
                { toMonthName = model.languageConfig.dateFormatLanguage.toMonthName
                , toMonthAbbreviation = model.languageConfig.dateFormatLanguage.toMonthAbbreviation
                , am = model.languageConfig.am
                , pm = model.languageConfig.pm
                , to = model.languageConfig.to
                , andBefore = model.languageConfig.beforeThisDate
                , andAfter = model.languageConfig.afterThisDate
                }
            , allowTime = allowTime
            }

        parseOutput =
            parseDateTime parseConfig model.inputText
    in
    case parseOutput of
        Ok value ->
            let
                ( selection, format ) =
                    convertInput zone value model
            in
            { model
                | selection = selection
                , inputText = prettyFormatSelection zone selection model.languageConfig format
                , visibleCalendarRange = getVisibleRangeFromSelection selection model.calendarType zone today
                , displayFormat = format
            }

        Err _ ->
            { model | inputText = prettyFormatSelection zone model.selection model.languageConfig model.displayFormat }


selectPresetInternal : PresetType -> Posix -> Zone -> Model -> ( Model, Cmd Msg )
selectPresetInternal presetType today zone model =
    let
        selection =
            PresetSelection presetType
    in
    R2.withCmd
        (Task.attempt (always DoNothing) <| Dom.focus "elm-fancy--daterangepicker--done")
        { model
            | isPresetMenuOpen = False
            , selection = selection
            , visibleCalendarRange = getVisibleRangeFromSelection selection model.calendarType zone today
            , inputText = prettyFormatSelection zone selection model.languageConfig model.displayFormat
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
                case model.dateSelectionType of
                    DateRangeSelection ->
                        R2.withNoCmd { model | isShiftDown = True }

                    DateSelection ->
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
                        selectPresetInternal (SelectList.selected a) today zone model

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

        BeforeSelection _ ->
            { start = changedValue, end = changedValue }

        AfterSelection _ ->
            { start = changedValue, end = changedValue }


cancelShift : Zone -> Model -> ( Model, Cmd Msg )
cancelShift zone model =
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
                    , inputText = prettyFormatSelection zone selection model.languageConfig model.displayFormat
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


updateCalendarRange : Model -> Int -> PartsRange -> ( Model, Cmd Msg )
updateCalendarRange model intervalChange currentVisibleRange =
    R2.withNoCmd
        { model
            | visibleCalendarRange =
                Just <|
                    calculateNewCalendarRange model intervalChange currentVisibleRange
        }


addMonthsToParts : Int -> Parts -> Parts
addMonthsToParts interval parts =
    { parts
        | month =
            parts.month
                |> monthToNumber
                |> (\x -> x + interval)
                |> (\y -> modBy 12 y)
                |> numberToMonth
                |> Maybe.withDefault Jan
    }


getFirstDayOfYearParts : Parts -> Parts
getFirstDayOfYearParts parts =
    { parts | month = Jan, day = 1 }


getLastDayOfYearParts : Parts -> Parts
getLastDayOfYearParts parts =
    { parts | month = Dec, day = 31 }


getFirstDayOfMonthStartOfDayParts : Parts -> Parts
getFirstDayOfMonthStartOfDayParts parts =
    { parts | day = 1 }
        |> getStartOfDayParts


getLastDayOfMonthEndOfDayParts : Parts -> Parts
getLastDayOfMonthEndOfDayParts parts =
    { parts | day = numberOfDaysInMonth parts.year parts.month }
        |> getEndOfDayParts


getStartOfDayParts : Parts -> Parts
getStartOfDayParts parts =
    { parts | hour = 0, minute = 0, second = 0, millisecond = 0 }


getEndOfDayParts : Parts -> Parts
getEndOfDayParts parts =
    { parts | hour = 23, minute = 59, second = 59, millisecond = 0 }


calculateNewCalendarRange : Model -> Int -> PartsRange -> PartsRange
calculateNewCalendarRange model intervalChange currentVisibleRange =
    let
        updateWithIntervalFunc intervalFunc range =
            { start = intervalFunc range.start
            , end = intervalFunc range.end
            }

        yearChange parts =
            { parts | year = Debug.log "new year" <| parts.year + intervalChange }
    in
    case model.calendarType of
        FullCalendar ->
            updateWithIntervalFunc yearChange currentVisibleRange

        ThreeMonths ->
            updateWithIntervalFunc (addMonthsToParts (intervalChange * 3)) currentVisibleRange

        TwoMonths ->
            updateWithIntervalFunc (addMonthsToParts (intervalChange * 2)) currentVisibleRange

        OneMonth ->
            updateWithIntervalFunc (addMonthsToParts intervalChange) currentVisibleRange


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


bottomBar : Model -> Zone -> Parts -> Html Msg
bottomBar model zone today =
    div [ Attrs.class "bottom-bar" ]
        [ button
            [ Attrs.id "elm-fancy--daterangepicker--done"
            , Attrs.class "done"
            , Html.Events.onClick <| Close (partsToPosix zone today)
            ]
            [ text model.languageConfig.done ]
        , button [ Attrs.class "reset", Html.Events.onClick Reset ]
            [ text model.languageConfig.reset ]
        ]


leftSelector : PartsRange -> Model -> Zone -> Html Msg
leftSelector =
    mkSelector -1 .end "prev-range-selector" "❮"


rightSelector : PartsRange -> Model -> Zone -> Html Msg
rightSelector =
    mkSelector 1 .start "next-range-selector" "❯"


mkSelector : Int -> (PartsRange -> Parts) -> String -> String -> PartsRange -> Model -> Zone -> Html Msg
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



{- Posix range is Local -> Today Posix is Local -}


topBar : Model -> PartsRange -> Parts -> Zone -> Html Msg
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
            case model.dateSelectionType of
                DateSelection ->
                    div [] []

                DateRangeSelection ->
                    clockButton model

        selection =
            { start = partsToPosix zone visibleRange.start, end = partsToPosix zone visibleRange.end }
                |> createSelectionInRange model zone
                |> RangeSelection
    in
    div [ Attrs.class class ]
        [ fullCalendarSelector
        , presetsDisplay model (partsToPosix zone today)
        , calendarInput model (partsToPosix zone today)
        , clock
        ]



-- View Function


createSelectionInRange : Model -> Zone -> PosixRange -> PosixRange
createSelectionInRange model zone posixRange =
    let
        startRange =
            model.availableForSelectionStart

        endRange =
            model.availableForSelectionEnd

        updatedSelectionStart =
            if posixToMillis posixRange.start < posixToMillis startRange then
                startRange

            else
                posixRange.start

        updatedSelectionEnd =
            if posixToMillis posixRange.end > posixToMillis endRange then
                endRange

            else
                posixRange.end
    in
    { start = updatedSelectionStart, end = updatedSelectionEnd }


selectionText : PartsRange -> String
selectionText visibleRange =
    String.fromInt visibleRange.start.year


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
    case model.dateSelectionType of
        DateRangeSelection ->
            model.languageConfig.dateRangePickerInputPlaceholder

        DateSelection ->
            model.languageConfig.datePickerInputPlaceholder


convertInput : Zone -> Input -> Model -> ( InternalSelection, Format )
convertInput zone input model =
    case input of
        SingleInput inputDate ->
            singleInputConversion zone inputDate model False

        RangeInput start end ->
            combineInputToRange zone start end

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

        BeforeInput inputDate ->
            singleInputConversion zone inputDate model True

        AfterInput inputDate ->
            singleInputConversion zone inputDate model False


singleInputConversion : Zone -> InputDate -> Model -> Bool -> ( InternalSelection, Format )
singleInputConversion zone inputDate model isEndSelection =
    let
        ( initialSelection, format ) =
            convertInputDate zone inputDate isEndSelection

        selection =
            case model.dateSelectionType of
                DateRangeSelection ->
                    case initialSelection of
                        SingleSelection posix ->
                            RangeSelection <| convertSingleIntoRange posix

                        _ ->
                            initialSelection

                DateSelection ->
                    initialSelection
    in
    ( selection, Maybe.withDefault DateFormat format )


combineInputToRange : Zone -> InputDate -> InputDate -> ( InternalSelection, Format )
combineInputToRange zone start end =
    let
        ( startSelection, startFormat ) =
            convertInputDate zone start False

        ( endSelection, endFormat ) =
            convertInputDate zone end True

        fullFormat =
            case ( startFormat, endFormat ) of
                ( Just DateTimeFormat, _ ) ->
                    DateTimeFormat

                ( _, Just DateTimeFormat ) ->
                    DateTimeFormat

                _ ->
                    DateFormat
    in
    case ( startSelection, endSelection ) of
        ( SingleSelection startPosix, SingleSelection endPosix ) ->
            ( RangeSelection { start = startPosix, end = endPosix }, fullFormat )

        ( SingleSelection startPosix, RangeSelection endPosixRange ) ->
            ( RangeSelection { start = startPosix, end = endPosixRange.end }, fullFormat )

        ( RangeSelection startPosixRange, SingleSelection endPosix ) ->
            ( RangeSelection { start = startPosixRange.start, end = endPosix }, fullFormat )

        ( RangeSelection startPosixRange, RangeSelection endPosixRange ) ->
            ( RangeSelection { start = startPosixRange.start, end = endPosixRange.end }, fullFormat )

        _ ->
            ( Unselected, DateFormat )


convertInputDate : Zone -> InputDate -> Bool -> ( InternalSelection, Maybe Format )
convertInputDate zone inputDate isEndSelection =
    case inputDate of
        JustYear year ->
            ( RangeSelection <| yearToPosixRange year zone, Nothing )

        JustYearAndMonth yearAndMonth ->
            ( RangeSelection <| yearAndMonthToPosixRange yearAndMonth zone, Nothing )

        FullDate dateParts ->
            let
                unAdjustedPosix =
                    datePartsToPosix dateParts zone

                adjusted =
                    if isEndSelection then
                        getEndOfDay unAdjustedPosix

                    else
                        unAdjustedPosix
            in
            ( SingleSelection adjusted, Nothing )

        FullDateTime dateTimeParts ->
            let
                unAdjustedPosix =
                    dateTimePartsToPosix dateTimeParts zone

                adjusted =
                    if isEndSelection then
                        addEndingDateTimeParts unAdjustedPosix

                    else
                        unAdjustedPosix
            in
            ( SingleSelection adjusted, Just DateTimeFormat )



{- Posix range is local -> Posix today is In UTC -}


calendarView : Model -> Parts -> PartsRange -> Zone -> Html Msg
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



{- Posix range is local -> Posix today is In UTC -}


yearCalendarView : Model -> Parts -> PartsRange -> Zone -> Html Msg
yearCalendarView model today visibleRange zone =
    let
        quarter name startMonth endMonth =
            let
                posixRangeForQuarter =
                    posixRangeForMonths startMonth endMonth visibleRange.start.year zone

                isOutOfRange =
                    posixIsOutOfAllowedRange (posixToParts zone posixRangeForQuarter.start) model zone
                        || posixIsOutOfAllowedRange (posixToParts zone posixRangeForQuarter.end) model zone

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
                List.map (\m -> monthCalendarView m today model zone) <|
                    getMonthsFromRange
                        0
                        11
                        visibleRange
                        getFirstDayOfYearParts
            ]
        ]



{- Posix range is local -> Posix today is In UTC -}


monthlyCalendarView : Model -> String -> Int -> Parts -> PartsRange -> Zone -> Html Msg
monthlyCalendarView model monthClass endInterval today visibleRange zone =
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "month-calendar" ]
        [ table []
            [ tbody [ Attrs.class monthClass ] <|
                List.map (\m -> monthCalendarView m today model zone)
                    (getMonthsFromRange 0 endInterval visibleRange getFirstDayOfMonthStartOfDayParts)
            ]
        ]


getMonthsFromRange : Int -> Int -> PartsRange -> (Parts -> Parts) -> List Parts
getMonthsFromRange start end visibleRange fn =
    List.map
        (\x ->
            addMonthsToParts x <| fn visibleRange.start
        )
    <|
        List.range start end


posixRangeForMonths : Month -> Month -> Int -> Zone -> PosixRange
posixRangeForMonths startMonth endMonth currentYear zone =
    let
        start =
            { year = currentYear
            , month = startMonth
            , day = 1
            , hour = 0
            , minute = 0
            , second = 0
            , millisecond = 0
            }

        end =
            { year = currentYear
            , month = endMonth
            , day = numberOfDaysInMonth currentYear endMonth
            , hour = 23
            , minute = 59
            , second = 59
            , millisecond = 0
            }
    in
    { start = partsToPosix zone start, end = partsToPosix zone end }



{- Posix Month is local -> Posix today is In UTC -}


monthCalendarView : Parts -> Parts -> Model -> Zone -> Html Msg
monthCalendarView currentMonth today model zone =
    let
        firstOfMonth =
            { currentMonth | day = 1, hour = 0, minute = 0, second = 0, millisecond = 0 }

        lastOfMonth =
            { currentMonth | day = numberOfDaysInMonth currentMonth.year currentMonth.month, hour = 23, minute = 59, second = 59, millisecond = 0 }

        range =
            { start = partsToPosix zone firstOfMonth
            , end = partsToPosix zone lastOfMonth
            }

        selectionInUtc =
            RangeSelection range

        wholeMonthIsOutOfRange =
            posixIsOutOfAllowedRange firstOfMonth model zone
                && posixIsOutOfAllowedRange lastOfMonth model zone

        attrs =
            if wholeMonthIsOutOfRange then
                [ Attrs.class "disabled", Attrs.class "month--header" ]

            else
                [ Attrs.class "month--header", onClick <| SetSelection selectionInUtc ]
    in
    td []
        [ table []
            [ thead attrs
                [ text <| monthFormatter model.languageConfig zone (partsToPosix zone currentMonth) ]
            , tbody [ Attrs.class "month" ] <|
                List.map (\x -> dayCalendarView zone currentMonth x today model) <|
                    getCurrentMonthDatesFullWeeks zone currentMonth
            ]
        ]



{- currentMonth is local, currrentDay is local, today is utc -}


dayCalendarView : Zone -> Parts -> Parts -> Parts -> Model -> Html Msg
dayCalendarView zone currentMonth currentDay today model =
    let
        wantedMonth =
            currentMonth.month

        contentIsInCorrectMonth =
            currentDay.month == wantedMonth

        ( hoverAttr, content, setDate ) =
            if contentIsInCorrectMonth then
                if posixIsOutOfAllowedRange currentDay model zone then
                    ( Attrs.class "", [ text <| String.fromInt currentDay.day ], Attrs.class "" )

                else
                    ( Html.Events.onMouseOver <| OnHoverOverDay (partsToPosix zone currentDay)
                    , [ text <| String.fromInt currentDay.day ]
                    , setDateAttr
                    )

            else
                ( Attrs.class "", [], Attrs.class "" )

        setDateAttr =
            if model.isShiftDown || model.isMouseDown then
                Just (partsToPosix zone currentDay) |> EndSelection |> onClickNoDefault

            else
                StartSelection (partsToPosix zone currentDay) |> DateRangePicker.Helper.mouseDownNoDefault

        isSameDayOfSelection posixFromSelection =
            contentIsInCorrectMonth && (Maybe.withDefault False <| Maybe.map (\p -> isSameDay p currentDay) posixFromSelection)

        --All local
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


calendarPositioning : Maybe Element -> Maybe Element -> WindowSize -> List (Attribute msg)
calendarPositioning buttonElement calendarElement size =
    case ( buttonElement, calendarElement ) of
        ( Just button, Just calendar ) ->
            [ calculateYPosition button calendar size
            , calculateXPosition button calendar size
            ]

        _ ->
            [ Attrs.style "left" "-9999px" ]


addPx : String -> String
addPx str =
    str ++ "px"


calculateYPosition : Element -> Element -> WindowSize -> Attribute msg
calculateYPosition button calendar { height } =
    let
        ( yNum, yName ) =
            if button.element.y < (height / 2) then
                ( additionalCalcForTop button.element.y, "top" )

            else
                ( additionalCalcForBottom (height - (button.element.y + button.element.height)), "bottom" )

        additionalCalcForBottom num =
            if button.element.y > calendar.element.height then
                num

            else
                num + (button.element.y - calendar.element.height - button.element.height)

        additionalCalcForTop num =
            if (height - button.element.y) > calendar.element.height then
                num

            else
                0
    in
    Attrs.style yName
        (yNum
            |> String.fromFloat
            |> addPx
        )


calculateXPosition : Element -> Element -> WindowSize -> Attribute msg
calculateXPosition button calendar { width } =
    let
        sideButtonRadius =
            15

        ( xNum, xName ) =
            if button.element.x > (width / 2) then
                ( additionalCalcForRight ((width - button.element.x) - button.element.width), "right" )

            else
                ( additionalCalcForLeft button.element.x, "left" )

        additionalCalcForLeft num =
            if button.element.x > calendar.element.width then
                num + (button.element.x - calendar.element.width - button.element.width)

            else
                num

        additionalCalcForRight num =
            if (width - button.element.x) > calendar.element.width then
                num + sideButtonRadius

            else
                num

        newNum =
            if xNum <= 15 then
                xNum + sideButtonRadius

            else
                xNum
    in
    Attrs.style xName
        (newNum
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


posixIsOutOfAllowedRange : Parts -> Model -> Zone -> Bool
posixIsOutOfAllowedRange parts model zone =
    let
        start =
            model.availableForSelectionStart

        end =
            model.availableForSelectionEnd
    in
    posixToMillis (partsToPosix zone parts) < posixToMillis start || posixToMillis (partsToPosix zone parts) > posixToMillis end


normalizeSelectingRange : PosixRange -> PosixRange
normalizeSelectingRange posixRange =
    if posixToMillis posixRange.start > posixToMillis posixRange.end then
        { start = posixRange.end, end = posixRange.start }

    else
        posixRange



--View Function
{- comparisonPosix is local, selection is utc, today is utc -}


selectionPoints : Parts -> Model -> Parts -> Zone -> ( Maybe Parts, Maybe Parts, Bool )
selectionPoints date { selection } today zone =
    let
        compareRange range =
            posixToMillis range.start
                <= (posixToMillis <| partsToPosix zone date)
                && (posixToMillis <| partsToPosix zone date)
                <= posixToMillis range.end

        -- let
        --     start =
        --         posixToParts zone range.start
        --     end =
        --         posixToParts zone range.end
        -- in
        -- (start.year <= date.year && end.year >= date.year)
        --     && (monthNum start <= monthNum date && monthNum end >= monthNum date)
        --     && (start.day <= date.day && end.day >= date.day)
    in
    -- we adjust the utc times to local for comparison
    case selection of
        SingleSelection posix ->
            ( Just <| posixToParts zone posix
            , Just <| posixToParts zone posix
            , False
            )

        RangeSelection posixRange ->
            ( Just <| posixToParts zone posixRange.start
            , Just <| Debug.log "end" <| posixToParts zone posixRange.end
            , compareRange posixRange
            )

        Unselected ->
            ( Nothing, Nothing, False )

        Selecting posixRange ->
            let
                normalized =
                    normalizeSelectingRange posixRange
            in
            ( Just <| posixToParts zone normalized.start
            , Just <| posixToParts zone normalized.end
            , compareRange normalized
            )

        PresetSelection presetType ->
            let
                posixRange =
                    presetToPosixRange presetType (partsToPosix zone today) zone
            in
            ( Just <| posixToParts zone posixRange.start
            , Just <| posixToParts zone posixRange.end
            , compareRange posixRange
            )

        AfterSelection posix ->
            ( Just <| posixToParts zone posix
            , Just <| posixToParts zone posix
            , False
            )

        BeforeSelection posix ->
            ( Just <| posixToParts zone posix
            , Just <| posixToParts zone posix
            , False
            )


isSameDay : Parts -> Parts -> Bool
isSameDay posix1 posix2 =
    posix1.day == posix2.day && posix1.month == posix2.month && posix1.year == posix2.year



{- today is local output is local -}


calcVisibleRange : Parts -> Model -> PartsRange
calcVisibleRange localToday model =
    case model.calendarType of
        FullCalendar ->
            { start =
                { localToday | month = Jan }
                    |> getFirstDayOfMonthStartOfDayParts
            , end =
                { localToday
                    | month = Dec
                }
                    |> getLastDayOfMonthEndOfDayParts
            }

        ThreeMonths ->
            { start =
                { localToday
                    | month = getPrevMonth localToday.month
                }
                    |> getFirstDayOfMonthStartOfDayParts
            , end =
                { localToday
                    | month = getNextMonth localToday.month
                }
                    |> getLastDayOfMonthEndOfDayParts
            }

        TwoMonths ->
            { start =
                { localToday | month = getPrevMonth localToday.month }
                    |> getFirstDayOfMonthStartOfDayParts
            , end =
                localToday
                    |> getLastDayOfMonthEndOfDayParts
            }

        OneMonth ->
            { start =
                localToday |> getFirstDayOfMonthStartOfDayParts
            , end =
                localToday |> getLastDayOfMonthEndOfDayParts
            }


convertToRange : Posix -> Zone -> CalendarType -> PartsRange
convertToRange day zone calendarType =
    let
        localToday =
            posixToParts zone day
    in
    case calendarType of
        FullCalendar ->
            { start = getFirstDayOfYearParts localToday, end = getLastDayOfYearParts localToday }

        ThreeMonths ->
            { start =
                { localToday
                    | month = getPrevMonth localToday.month
                }
                    |> getFirstDayOfMonthStartOfDayParts
            , end =
                { localToday
                    | month = getNextMonth localToday.month
                }
                    |> getLastDayOfMonthEndOfDayParts
            }

        TwoMonths ->
            { start =
                { localToday | month = getPrevMonth localToday.month }
                    |> getFirstDayOfMonthStartOfDayParts
            , end =
                localToday
                    |> getLastDayOfMonthEndOfDayParts
            }

        OneMonth ->
            { start =
                localToday |> getFirstDayOfMonthStartOfDayParts
            , end =
                localToday |> getLastDayOfMonthEndOfDayParts
            }



{- internal Selection is in utc, outout posix range is in local -}


getVisibleRangeFromSelection : InternalSelection -> CalendarType -> Zone -> Posix -> Maybe PartsRange
getVisibleRangeFromSelection selection calendarType zone today =
    case selection of
        SingleSelection posix ->
            Just { start = getStartOfDayParts <| posixToParts zone posix, end = getEndOfDayParts <| posixToParts zone posix }

        RangeSelection posixRange ->
            convertToRange posixRange.start zone calendarType
                |> Just

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        PresetSelection presetType ->
            presetToPosixRange presetType today zone
                |> (\{ start, end } -> PartsRange (posixToParts zone start) (posixToParts zone end))
                |> Just

        BeforeSelection posix ->
            Just { start = getStartOfDayParts <| posixToParts zone posix, end = getEndOfDayParts <| posixToParts zone posix }

        AfterSelection posix ->
            Just { start = getStartOfDayParts <| posixToParts zone posix, end = getEndOfDayParts <| posixToParts zone posix }


prettyFormatSelection : Zone -> InternalSelection -> LanguageConfig -> Format -> String
prettyFormatSelection zone selection language format =
    case selection of
        SingleSelection posix ->
            singleFormatter zone language format posix

        RangeSelection posixRange ->
            if isSameDay (posixToParts zone posixRange.start) (posixToParts zone posixRange.end) && format /= DateTimeFormat then
                singleFormatter zone language format posixRange.start

            else
                fullFormatter zone language format posixRange.start posixRange.end

        Unselected ->
            ""

        Selecting _ ->
            ""

        PresetSelection presetType ->
            presetToDisplayString presetType language

        BeforeSelection posix ->
            starFormatter language format language.beforeThisDate zone posix

        AfterSelection posix ->
            starFormatter language format language.afterThisDate zone posix


singleFormatter : Zone -> LanguageConfig -> Format -> Posix -> String
singleFormatter zone language format =
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
        zone


starFormatter : LanguageConfig -> Format -> String -> Zone -> Posix -> String
starFormatter language format additionalString zone =
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

        dateString pos =
            DateFormat.formatWithLanguage language.dateFormatLanguage
                ([ DateFormat.monthNameAbbreviated
                 , DateFormat.text " "
                 , DateFormat.dayOfMonthNumber
                 , DateFormat.text ", "
                 , DateFormat.yearNumber
                 ]
                    ++ timeParts
                )
                zone
                pos
                ++ additionalString
    in
    dateString


monthFormatter : LanguageConfig -> Zone -> Posix -> String
monthFormatter language =
    DateFormat.formatWithLanguage language.dateFormatLanguage
        [ DateFormat.monthNameFull
        ]


fullFormatter : Zone -> LanguageConfig -> Format -> Posix -> Posix -> String
fullFormatter zone language format start end =
    singleFormatter zone language format start
        ++ " to "
        ++ singleFormatter zone language format end



-- Copied from Derberos.Date.Utils and edited to make sunday first day of the week


getCurrentMonthDatesFullWeeks : Zone -> Parts -> List Parts
getCurrentMonthDatesFullWeeks zone time =
    let
        firstDayOfMonth =
            time
                |> getFirstDayOfMonthStartOfDayParts
                |> partsToPosix zone
                |> (\x -> TimeExtra.add TimeExtra.Day (getDiffLastSun (getWeekday zone x)) zone x)

        lastDayOfMonth =
            time
                |> getLastDayOfMonthEndOfDayParts
                |> partsToPosix zone
                |> (\x -> TimeExtra.add TimeExtra.Day (getDiffNextSat (getWeekday zone x)) zone x)

        numberDaysInMonth =
            (posixToMillis lastDayOfMonth - posixToMillis firstDayOfMonth) // (1000 * 60 * 60 * 24)

        -- numberOfDaysInMonth time.year time.month
        addDayParts posix i =
            TimeExtra.add TimeExtra.Day i zone posix
    in
    List.range 0 numberDaysInMonth
        |> List.map (addDayParts firstDayOfMonth)
        |> List.map (posixToParts zone)


getDiffLastSun : Weekday -> Int
getDiffLastSun weekday =
    case weekday of
        Mon ->
            -1

        Tue ->
            -2

        Wed ->
            -3

        Thu ->
            -4

        Fri ->
            -5

        Sat ->
            -6

        Sun ->
            0


getDiffNextSat : Weekday -> Int
getDiffNextSat weekday =
    case weekday of
        Mon ->
            5

        Tue ->
            4

        Wed ->
            3

        Thu ->
            2

        Fri ->
            1

        Sat ->
            0

        Sun ->
            6


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


addEndingDateTimeParts : Posix -> Posix
addEndingDateTimeParts posix =
    let
        dateRecord =
            posixToCivil posix

        updatedDateRecord =
            { dateRecord
                | second = 59
                , millis = 99
            }
    in
    civilToPosix updatedDateRecord


checkForChange : (DatePicker -> Maybe a) -> DatePicker -> Maybe a -> Bool
checkForChange checkFunc model elementForComparison =
    if isOpen model then
        False

    else
        checkFunc model /= elementForComparison


convertSingleIntoRange : Posix -> PosixRange
convertSingleIntoRange posix =
    let
        timeHasBeenSetByUser =
            posix /= getStartOfDay posix
    in
    if timeHasBeenSetByUser then
        { start = posix, end = addEndingDateTimeParts posix }

    else
        { start = getStartOfDay posix, end = getEndOfDay posix }



--------------------------------------------------------------------------------------------
--            PRESETS
--------------------------------------------------------------------------------------------


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
