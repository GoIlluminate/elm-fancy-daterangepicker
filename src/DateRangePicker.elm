module DateRangePicker exposing
    ( Msg, DatePicker, subscriptions, view, update
    , open, defaultOpener
    , Selection(..), Format(..), PosixRange, PartsRange, getSelection, getSelectionRange
    , Config, LanguageConfig, englishLanguageConfig, DateSelectionType(..), PresetType(..), Interval(..), CustomPreset, CalendarType(..), defaultConfig, initWithOptions, updateModelWithConfig
    , setCalendarType, presets, setOpen, setSelection, languageConfig, selectPreset, displayFormat
    , partsRangeToPosixRange, presetToDisplayString, hasRangeChanged, hasSelectionChanged, presetToPartsRange, displaySelection, displaygetSelection
    , ClockStyle(..), DatePickerVisibility(..), defaultPresets, focusInput, getConfig, getEndOfDayParts, getStartOfDayParts, isOpen, openMsg, resetModel, setDisplayFormat
    )

{-| A customizable date picker component.


# Basics

@docs Msg, DatePicker, subscriptions, view, update

@docs init, open, defaultOpener


# Selection

@docs Selection, Format, PosixRange, PartsRange, getSelection, getSelectionRange, getSelectionSingle


# Settings

@docs Config, LanguageConfig, englishLanguageConfig, DateSelectionType, PresetType, Interval, CustomPreset, CalendarType, defaultConfig, initWithOptions, updateModelWithConfig


# Model Helpers

@docs setCalendarType, datepickerVisibility, presets, setOpen, setSelection, languageConfig, selectPreset, displayFormat


# Helpers

@docs partsRangeToPosixRange, presetToDisplayString, hasRangeChanged, hasSelectionChanged, presetToPartsRange, selectPreset, displaySelection, displaygetSelection

-}

import Browser.Dom as Dom exposing (Element, Error, Viewport, getElement)
import Browser.Events
import DateFormat exposing (Token)
import DateFormat.Language as DateFormat
import DateRangePicker.DateRecordParser exposing (Input(..), InputDate(..), YearAndMonth, datePartsToParts, dateTimePartsToParts, parseDateTime, yearAndMonthToParts, yearToParts)
import DateRangePicker.Helper exposing (onClickNoDefault)
import Derberos.Date.Calendar exposing (getCurrentMonthDatesFullWeeks)
import Derberos.Date.Utils exposing (getNextMonth, getPrevMonth, getWeekday, numberOfDaysInMonth)
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
    | OpenDatePicker String
    | CloseDatePicker
    | SetVisibleRange PartsRange
    | SetSelection Selection
    | OnInputFinish
    | OnInputChange String
    | Reset
    | StartSelection Parts
    | EndSelection (Maybe Parts)
    | KeyDown RawKey
    | KeyUp RawKey
    | CancelShift
    | OnHoverOverDay Parts
    | MouseOutsideOfCalendar
    | OnMouseMove Mouse.Event
    | OnMouseUp
    | SetMouseOutside Bool
    | OnGetElementSuccess (Result Error Element)
    | OnGetDatePickerButton (Result Error Element)
    | CheckToMoveToNextVisibleRange
    | SetPresetMenuVisibility MenuVisibility
    | SelectPreset PresetType
    | ToggleFormat
    | OnWindowsResize Int Int
    | GotViewPort (Result Error Viewport)
    | GotTime Posix


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


defaultPresets : List PresetType
defaultPresets =
    [ Today
    , Yesterday
    , PastWeek
    , PastMonth
    , PastYear
    ]


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


{-| The type which represents what the current selection is in the datepicker.

If you select a preset you can use @presetToPartsRange to get the appropriate posix range for the selection.

-}
type Selection
    = Single Parts
    | Range PartsRange
    | Unselected
    | Selecting PartsRange
    | Preset PresetType
    | Before Parts
    | After Parts


{-| The type which specifies what size calendar you want to display
-}
type CalendarType
    = YearCalendar
    | ThreeMonthCalendar
    | TwoMonthCalendar
    | OneMonthCalendar


type DateSelectionType
    = DateRangeSelection
    | DateSelection


{-| The type which specifies if a user had specified a time in the input box as well as the selected date.
-}
type Format
    = DateFormat
    | DateTimeFormat ClockStyle


type ClockStyle
    = H24
    | H12


type MenuVisibility
    = MenuOpen
    | MenuClosed


type alias WindowSize =
    { width : Float
    , height : Float
    }


type alias PartsRange =
    { start : Parts
    , end : Parts
    }


type UserDateInput
    = DirtyInput String
    | CommittedInput String


getUserDateInput : UserDateInput -> String
getUserDateInput input =
    case input of
        DirtyInput str ->
            str

        CommittedInput str ->
            str


type DatePickerVisibility
    = Open
    | Closed
    | AlwaysOpen


{-| A record which represents the main datepicker model
-}
type alias Model =
    { selection : Selection
    , allowedRange : PartsRange
    , visibleCalendarRange : PartsRange
    , isMouseDown : Bool
    , isShiftDown : Bool
    , isMouseOutside : Bool
    , hidePresets : Bool
    , canChooseTime : Bool
    , datepickerVisibility : DatePickerVisibility
    , presets : List PresetType
    , calendarType : CalendarType
    , inputText : UserDateInput
    , currentlyHoveredDate : Maybe Parts
    , mousePosition : Maybe Mouse.Event
    , uiElement : Maybe Element
    , uiButton : Maybe Element
    , windowSize : WindowSize
    , languageConfig : LanguageConfig
    , presetMenuVisibility : MenuVisibility
    , keyboardSelectedPreset : Maybe (SelectList PresetType)
    , displayFormat : Format
    , dateSelectionType : DateSelectionType
    , displayTimezone : Zone
    , now : Posix
    , clockStyle : ClockStyle
    , yPadding : Maybe Int
    , displayDate : Maybe Posix
    }


{-| A record which represents the main datepicker model
-}
type DatePicker
    = DatePicker Model


{-| Initialize the datepicker with the default settings
-}
initWithOptions : Posix -> Maybe Posix -> Config -> DatePicker
initWithOptions now displayDate config =
    DatePicker <|
        { visibleCalendarRange = getStartingVisibleRange config.displayTimezone now displayDate config.calendarType
        , isMouseDown = False
        , isShiftDown = False
        , inputText = CommittedInput ""
        , currentlyHoveredDate = Nothing
        , mousePosition = Nothing
        , uiElement = Nothing
        , uiButton = Nothing
        , isMouseOutside = False
        , canChooseTime = config.canChooseTime
        , languageConfig = config.languageConfig
        , presetMenuVisibility = MenuClosed
        , keyboardSelectedPreset = Nothing
        , displayFormat = DateFormat
        , windowSize = WindowSize 0 0
        , now = now
        , allowedRange = config.allowedRange
        , presets = config.presets
        , calendarType = config.calendarType
        , datepickerVisibility = config.datepickerVisibility
        , dateSelectionType = config.dateSelectionType
        , hidePresets = config.hidePresets
        , selection = selectionToSelection config.defaultSelection
        , displayTimezone = config.displayTimezone
        , clockStyle = config.clockStyle
        , yPadding = config.yPadding
        , displayDate = displayDate
        }


getStartingVisibleRange : Zone -> Posix -> Maybe Posix -> CalendarType -> PartsRange
getStartingVisibleRange zone now displayDate calendarType =
    let
        getVisibleRange date =
            calcVisibleRange (posixToParts zone date) calendarType
    in
    case displayDate of
        Just dd ->
            getVisibleRange dd

        Nothing ->
            getVisibleRange now


{-| A record which specifies config options which can be set when initializes the datepicker
-}
type alias Config =
    { allowedRange : PartsRange
    , presets : List PresetType
    , calendarType : CalendarType
    , datepickerVisibility : DatePickerVisibility
    , languageConfig : LanguageConfig
    , dateSelectionType : DateSelectionType
    , canChooseTime : Bool
    , hidePresets : Bool
    , defaultSelection : Maybe Selection
    , displayTimezone : Zone
    , displayDate : Maybe Posix
    , clockStyle : ClockStyle
    , yPadding : Maybe Int
    }


{-| Gets the current languageConfig
-}
getConfig : DatePicker -> Config
getConfig (DatePicker model) =
    { allowedRange = model.allowedRange
    , presets = model.presets
    , calendarType = model.calendarType
    , datepickerVisibility = model.datepickerVisibility
    , languageConfig = model.languageConfig
    , dateSelectionType = model.dateSelectionType
    , canChooseTime = model.canChooseTime
    , hidePresets = model.hidePresets
    , defaultSelection = Just model.selection
    , displayTimezone = model.displayTimezone
    , displayDate = Just model.now
    , clockStyle = model.clockStyle
    , yPadding = model.yPadding
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
    , displayFormat : List Token
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
    , displayFormat = ([ DateFormat.monthNameAbbreviated
                               , DateFormat.text " "
                               , DateFormat.dayOfMonthNumber
                               , DateFormat.text ", "
                               , DateFormat.yearNumber
                               ]
                              )
    }


defaultAllowedRange : PartsRange
defaultAllowedRange =
    { start = Parts 1900 Jan 1 0 0 0 0, end = Parts 2100 Dec 31 23 59 59 0 }


{-| A default config which can be combined with @initWithOptions so that you only need to specify the fields which you want to customize
-}
defaultConfig : Config
defaultConfig =
    { allowedRange = defaultAllowedRange
    , presets = defaultPresets
    , calendarType = YearCalendar
    , datepickerVisibility = Closed
    , languageConfig = englishLanguageConfig
    , dateSelectionType = DateRangeSelection
    , hidePresets = False
    , defaultSelection = Nothing
    , displayTimezone = Time.utc
    , displayDate = Nothing
    , canChooseTime = True
    , clockStyle = H24
    , yPadding = Nothing
    }


{-| A helper attribute which allows you to open the datepicker using any html element.

    The element you place this attribute on has to have an id so that the datepicker can dynamically be placed.

    Html.map DatePickerMsgs <| button [Html.Attribute.id openerId, open openerId ] [ text "Open Me!" ]

You will need to call convert the message to the appropriate type via Html.map

-}
open : String -> Attribute Msg
open openerId =
    Html.Events.onClick (OpenDatePicker openerId)



{-
   The open date picker msg so you can open the date picker with a custom event
-}


openMsg : String -> Msg
openMsg openerId =
    OpenDatePicker openerId


{-| A pre-made opener that can be used to open the datepicker instead of @open.

    It will need an id so that the datepicker can dynamically be placed.

    Html.map DatePickerMsgs <| defaultOpener model.datePicker openerId

You will need to call convert the message to the appropriate type via Html.map

-}
defaultOpener : DatePicker -> String -> Html Msg
defaultOpener (DatePicker model) openerId =
    let
        selectionValue =
            prettyFormatSelection (DatePicker model)

        displayValue =
            if String.isEmpty selectionValue then
                inputPlaceHolder model

            else
                selectionValue
    in
    button
        [ Attrs.class "elm-fancy--daterangepicker--opener"
        , Keyboard.Events.on Keypress [ ( Enter, OpenDatePicker openerId ) ]
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
        DateRangePicker.subscriptions model.datePicker

-}
subscriptions : DatePicker -> Sub Msg
subscriptions (DatePicker model) =
    let
        shiftSubs =
            if model.isShiftDown then
                [ Keyboard.ups KeyUp
                , Browser.Events.onVisibilityChange (always CancelShift)
                ]

            else
                []

        mouseSubs =
            if model.isMouseDown then
                [ Browser.Events.onMouseUp (EndSelection model.currentlyHoveredDate |> Json.succeed)
                , Browser.Events.onVisibilityChange (EndSelection model.currentlyHoveredDate |> always)
                , Time.every 750 (always <| CheckToMoveToNextVisibleRange)
                ]

            else
                []

        mouseMove =
            if model.isMouseOutside && model.isMouseDown then
                [ Browser.Events.onMouseMove (Json.map OnMouseMove Mouse.eventDecoder)
                , Browser.Events.onMouseUp (Json.succeed OnMouseUp)
                ]

            else
                []

        alwaysSubbed =
            [ Browser.Events.onResize OnWindowsResize, Time.every (60 * 1000) GotTime ]

        closeSub =
            if model.isMouseOutside && not model.isMouseDown then
                [ Browser.Events.onClick (Json.succeed CloseDatePicker) ]

            else
                []
    in
    case model.datepickerVisibility of
        Open ->
            List.concat [ shiftSubs, mouseSubs, [ Keyboard.downs KeyDown ], closeSub, alwaysSubbed ]
                |> Sub.batch

        AlwaysOpen ->
            List.concat [ shiftSubs, mouseSubs, [ Keyboard.downs KeyDown ], closeSub, alwaysSubbed, mouseMove ]
                |> Sub.batch

        Closed ->
            alwaysSubbed
                |> Sub.batch


{-| The update for the datepicker. You will need to integrate this into your own update.

    DatePickerMsgs msg_ ->
        let
            ( newDateRangePicker, dateRangePickerCmd ) =
                DateRangePicker.update msg_ model.datePicker
        in
        ( { model | datePicker = newDateRangePicker }, Cmd.map DatePickerMsgs dateRangePickerCmd )

-}
update : Msg -> DatePicker -> ( DatePicker, Cmd Msg )
update msg (DatePicker model) =
    let
        ( model_, cmd ) =
            innerUpdate msg model
    in
    ( DatePicker model_, cmd )


{-| The view for the datepicker. You will have to pass in the current time as well as the local zone and the datepicker model.

    DateRangePicker.view datePicker

-}
view : DatePicker -> Html Msg
view (DatePicker model) =
    let
        -- All view Functions from here on Take in a Time.Extra.Parts which is in local time
        outsideMouseEvent =
            if model.isMouseDown && model.isMouseOutside then
                Mouse.onMove OnMouseMove

            else
                Attrs.class ""

        calendarAttrs =
            List.append
                [ Attrs.class "elm-fancy--daterangepicker-body"
                , Attrs.id "elm-fancy--daterangepicker--wrapper"
                , outsideMouseEvent
                ]
                (if model.datepickerVisibility == Open then
                    calendarPositioning model.uiButton model.uiElement model.windowSize model.yPadding

                 else
                    []
                )
    in
    case model.datepickerVisibility of
        AlwaysOpen ->
            div
                [ Attrs.class "elm-fancy--daterangepicker elm-fancy--daterangepicker--always-open"
                , Html.Events.onMouseLeave <| SetMouseOutside True
                , Html.Events.onMouseEnter <| SetMouseOutside False
                ]
                [ div
                    calendarAttrs
                    [ topBar model
                    , leftSelector model
                    , rightSelector model
                    , calendarView model
                    , bottomBar model
                    ]
                ]

        Open ->
            div [ Attrs.class "elm-fancy--daterangepicker" ]
                [ div
                    [ Attrs.class "elm-fancy--daterangepicker-close"
                    , outsideMouseEvent
                    , Html.Events.onMouseLeave <| SetMouseOutside False
                    , Html.Events.onMouseEnter <| SetMouseOutside True
                    ]
                    []
                , div
                    calendarAttrs
                    [ topBar model
                    , leftSelector model
                    , rightSelector model
                    , calendarView model
                    , bottomBar model
                    ]
                ]

        Closed ->
            div [ Attrs.class "elm-fancy--daterangepicker" ]
                [ div
                    calendarAttrs
                    []
                ]


getTodayParts : Model -> Parts
getTodayParts model =
    posixToParts model.displayTimezone model.now


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
selectPreset : PresetType -> DatePicker -> DatePicker
selectPreset presetType (DatePicker model) =
    let
        model_ =
            selectPresetInternal presetType model
    in
    DatePicker model_


{-| Checks if the datepicker is open
-}
isOpen : DatePicker -> Bool
isOpen (DatePicker model) =
    model.datepickerVisibility == Open || model.datepickerVisibility == AlwaysOpen


{-| Gets the current selection format
-}
displayFormat : DatePicker -> Format
displayFormat (DatePicker model) =
    model.displayFormat


{-| sets the current selection format
-}
setDisplayFormat : Format -> DatePicker -> DatePicker
setDisplayFormat format (DatePicker model) =
    DatePicker { model | displayFormat = format }


{-| Set whether or not the datepicker is open. Usually you should use @open or @defaultOpener to manage this.
-}
setOpen : DatePicker -> DatePickerVisibility -> DatePicker
setOpen (DatePicker model) setIsOpen =
    DatePicker
        { model
            | datepickerVisibility = setIsOpen
        }


{-| Check if the selection has changed.
-}
hasSelectionChanged : DatePicker -> Maybe Selection -> Bool
hasSelectionChanged model comparisonSelection =
    checkForChange (Just << getSelection) model comparisonSelection


{-| Check if the selection has changed.
-}
hasRangeChanged : DatePicker -> Maybe PartsRange -> Bool
hasRangeChanged model comparisonRange =
    checkForChange getSelectionRange model comparisonRange


{-| A helper function to change the calendar type on an existing model. Usually you should use @initWithOptions and configure this at initialization.
-}
setCalendarType : CalendarType -> DatePicker -> DatePicker
setCalendarType calendarType (DatePicker model) =
    DatePicker { model | calendarType = calendarType }


{-| Get the current selection in local time.
-}
getSelection : DatePicker -> Selection
getSelection (DatePicker { selection }) =
    selection


{-| A convenience function to get the current selection as a parts range in local time.
-}
getSelectionRange : DatePicker -> Maybe PartsRange
getSelectionRange (DatePicker model) =
    case model.selection of
        Single pos ->
            Just <| convertSingleIntoRange pos

        Range range ->
            Just range

        Unselected ->
            Nothing

        Selecting _ ->
            Nothing

        Preset presetType ->
            Just <| presetToPartsRange (DatePicker model) presetType

        Before pos ->
            Just <| convertSingleIntoRange pos

        After pos ->
            Just <| convertSingleIntoRange pos


{-| A convenience function to get the current selection as a single parts in local time. This is particularly useful when you only allow for single date selection.
-}
getSelectionSingle : DatePicker -> Maybe Parts
getSelectionSingle (DatePicker model) =
    Maybe.map .start (getSelectionRange (DatePicker model))


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
            | allowedRange = config.allowedRange
            , presets = config.presets
            , calendarType = config.calendarType
            , datepickerVisibility = config.datepickerVisibility
            , dateSelectionType = config.dateSelectionType
            , hidePresets = config.hidePresets
            , selection = selectionToSelection config.defaultSelection
            , displayTimezone = config.displayTimezone
            , canChooseTime = config.canChooseTime
            , clockStyle = config.clockStyle
        }


{-| Change the datePicker's current config. This will reset changed state.
-}
resetModel : DatePicker -> DatePicker
resetModel (DatePicker model) =
    DatePicker
        { model
            | selection = Unselected
            , inputText = CommittedInput ""
            , visibleCalendarRange = getStartingVisibleRange model.displayTimezone model.now model.displayDate model.calendarType
        }


{-| Sets the datepicker's selection outside of the ui. Normally the ui should have all the interactions you would want.
-}
setSelection : Selection -> DatePicker -> DatePicker
setSelection selection (DatePicker model) =
    let
        newSelection =
            case selection of
                Before parts ->
                    Before <| getEndOfDayParts parts

                After parts ->
                    After <| getStartOfDayParts parts

                _ ->
                    selection

        updated =
            withUpdatedSelection newSelection model

        withUpdatedViewRange =
            { updated | visibleCalendarRange = getVisibleRangeFromSelection selection model.calendarType (DatePicker updated) }
    in
    DatePicker withUpdatedViewRange

{-| A helper function to display the selection in the same way that the datepicker does
-}
displaySelection : DatePicker -> String
displaySelection datePicker =
    prettyFormatSelection datePicker


{-| A helper function to display the selection in the same way that the datepicker does. It is in utc time.
-}
displaygetSelection : DatePicker -> String
displaygetSelection datePicker =
    prettyFormatSelectionInZone datePicker Time.utc



{- focuses the inputbox -}


focusInput : Cmd Msg
focusInput =
    Task.attempt (always DoNothing) <| Dom.focus "elm-fancy--daterangepicker--input"


innerUpdate : Msg -> Model -> ( Model, Cmd Msg )
innerUpdate msg model =
    case msg of
        DoNothing ->
            R2.withNoCmd model

        OpenDatePicker buttonId ->
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
                model

        CloseDatePicker ->
            let
                newVisibility =
                    if model.datepickerVisibility == AlwaysOpen then
                        AlwaysOpen

                    else
                        Closed
            in
            R2.withNoCmd <| finishInput { model | datepickerVisibility = newVisibility, uiButton = Nothing, uiElement = Nothing, isMouseOutside = False }

        SetVisibleRange visibleCalendarRange ->
            R2.withNoCmd
                { model
                    | visibleCalendarRange = visibleCalendarRange
                }

        SetSelection selection ->
            withUpdatedSelection selection model
                |> R2.withNoCmd

        OnInputFinish ->
            R2.withNoCmd <| finishInput model

        OnInputChange newText ->
            R2.withNoCmd { model | inputText = DirtyInput newText }

        Reset ->
            R2.withNoCmd
                { model
                    | inputText = CommittedInput ""
                    , selection = Unselected
                    , visibleCalendarRange = getStartingVisibleRange model.displayTimezone model.now model.displayDate model.calendarType
                }

        StartSelection posix ->
            case model.dateSelectionType of
                DateSelection ->
                    withUpdatedSelection (Single posix) model
                        |> R2.withNoCmd

                DateRangeSelection ->
                    withUpdatedSelection (Selecting { start = posix, end = posix }) { model | isMouseDown = True }
                        |> R2.withCmd
                            (Task.attempt OnGetElementSuccess <|
                                getElement "elm-fancy--daterangepicker--wrapper"
                            )

        EndSelection posix ->
            case posix of
                Just date ->
                    let
                        selection =
                            date
                                |> getEndOfDayParts
                                |> createSelectingRange model
                                |> normalizeSelectingRange
                                |> Range
                    in
                    withUpdatedSelection selection { model | isMouseDown = False }
                        |> R2.withNoCmd

                Nothing ->
                    R2.withNoCmd model

        KeyDown rawKey ->
            onKey rawKey model (onKeyDown model)

        KeyUp rawKey ->
            onKey rawKey
                model
                (\key ->
                    if key == Shift then
                        cancelShift model

                    else
                        R2.withNoCmd model
                )

        CancelShift ->
            cancelShift model

        OnHoverOverDay posix ->
            let
                withNewSelection =
                    if model.isMouseDown || model.isShiftDown then
                        withUpdatedSelection (Selecting <| createSelectingRange model posix) { model | currentlyHoveredDate = Just posix }

                    else
                        { model | currentlyHoveredDate = Just posix }
            in
            R2.withNoCmd withNewSelection

        MouseOutsideOfCalendar ->
            R2.withNoCmd { model | currentlyHoveredDate = Nothing }

        OnMouseMove event ->
            R2.withNoCmd { model | mousePosition = Just event }

        OnMouseUp ->
            R2.withNoCmd { model | isMouseDown = False }

        SetMouseOutside bool ->
            R2.withNoCmd { model | isMouseOutside = bool }

        OnGetElementSuccess result ->
            case result of
                Ok element ->
                    let
                        newModel =
                            { model | uiElement = Just element }

                        newVisibility =
                            if model.datepickerVisibility == AlwaysOpen then
                                AlwaysOpen

                            else
                                Open
                    in
                    if checkIfPositioningPossible newModel then
                        R2.withNoCmd { newModel | datepickerVisibility = newVisibility }

                    else
                        R2.withNoCmd newModel

                Err _ ->
                    R2.withNoCmd model

        OnGetDatePickerButton result ->
            case result of
                Ok element ->
                    let
                        newModel =
                            { model | uiButton = Just element }

                        newVisibility =
                            if model.datepickerVisibility == AlwaysOpen then
                                AlwaysOpen

                            else
                                Open
                    in
                    if checkIfPositioningPossible newModel then
                        R2.withNoCmd { newModel | datepickerVisibility = newVisibility }

                    else
                        R2.withNoCmd newModel

                Err _ ->
                    R2.withNoCmd model

        CheckToMoveToNextVisibleRange ->
            case ( model.uiElement, model.mousePosition, model.isMouseOutside ) of
                ( Just element, Just position, True ) ->
                    case calculateMousePosition element position of
                        OutsideRight ->
                            updateCalendarRange model 1

                        OutsideLeft ->
                            updateCalendarRange model -1

                        _ ->
                            R2.withNoCmd model

                _ ->
                    R2.withNoCmd model

        SetPresetMenuVisibility visibility ->
            R2.withNoCmd { model | presetMenuVisibility = visibility, keyboardSelectedPreset = Nothing }

        SelectPreset presetType ->
            selectPresetInternal presetType model
                |> R2.withNoCmd

        ToggleFormat ->
            let
                toggleFormat =
                    case model.displayFormat of
                        DateFormat ->
                            { model
                                | displayFormat = DateTimeFormat model.clockStyle
                            }

                        DateTimeFormat _ ->
                            { model
                                | displayFormat = DateFormat
                            }

                updateInputText =
                    { toggleFormat | inputText = CommittedInput (prettyFormatSelection (DatePicker toggleFormat)) }
            in
            R2.withNoCmd updateInputText

        OnWindowsResize width height ->
            R2.withNoCmd { model | windowSize = WindowSize (toFloat width) (toFloat height) }

        GotViewPort (Result.Ok viewPort) ->
            R2.withNoCmd { model | windowSize = WindowSize viewPort.viewport.width viewPort.viewport.height }

        GotViewPort _ ->
            R2.withNoCmd model

        GotTime now ->
            R2.withNoCmd { model | now = now }


withUpdatedSelection : Selection -> Model -> Model
withUpdatedSelection selection model =
    let
        updatedModel =
            { model | selection = selection }
    in
    { updatedModel | inputText = CommittedInput (prettyFormatSelection (DatePicker updatedModel)) }


selectionToSelection : Maybe Selection -> Selection
selectionToSelection selection =
    Maybe.withDefault Unselected
        selection


finishInput : Model -> Model
finishInput model =
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

        parseInput inputText =
            case parseDateTime parseConfig inputText of
                Ok value ->
                    let
                        ( selection, format ) =
                            convertInput value model

                        modelWithFormat =
                            withUpdatedSelection selection { model | displayFormat = format }
                    in
                    { modelWithFormat
                        | visibleCalendarRange = getVisibleRangeFromSelection selection model.calendarType (DatePicker modelWithFormat)
                    }

                Err _ ->
                    withUpdatedSelection model.selection model
    in
    case model.inputText of
        DirtyInput inputText ->
            parseInput inputText

        CommittedInput _ ->
            model


selectPresetInternal : PresetType -> Model -> Model
selectPresetInternal presetType model =
    let
        selection =
            Preset presetType

        updatedModel =
            withUpdatedSelection
                selection
                { model
                    | presetMenuVisibility = MenuClosed
                    , visibleCalendarRange = getVisibleRangeFromSelection selection model.calendarType (DatePicker model)
                    , keyboardSelectedPreset = Nothing
                }
    in
    updatedModel


onKeyDown : Model -> Key -> ( Model, Cmd Msg )
onKeyDown model key =
    case key of
        Shift ->
            case model.dateSelectionType of
                DateRangeSelection ->
                    case model.currentlyHoveredDate of
                        Just date ->
                            withUpdatedSelection (Selecting <| createSelectingRange { model | isShiftDown = True } date) { model | isShiftDown = True }
                                |> R2.withNoCmd

                        Nothing ->
                            { model | isShiftDown = True }
                                |> R2.withNoCmd

                DateSelection ->
                    R2.withNoCmd model

        Escape ->
            if model.presetMenuVisibility == MenuOpen then
                R2.withNoCmd { model | presetMenuVisibility = MenuClosed, keyboardSelectedPreset = Nothing }

            else
                R2.withNoCmd
                    { model
                        | datepickerVisibility =
                            if model.datepickerVisibility == AlwaysOpen then
                                AlwaysOpen

                            else
                                Closed
                        , uiButton = Nothing
                        , uiElement = Nothing
                    }

        ArrowDown ->
            arrowMovement model 1 SelectList.fromList

        ArrowUp ->
            arrowMovement model -1 createSelectListWithLast

        Enter ->
            if model.presetMenuVisibility == MenuOpen then
                case model.keyboardSelectedPreset of
                    Just a ->
                        selectPresetInternal (SelectList.selected a) model
                            |> R2.withNoCmd

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
    if model.presetMenuVisibility == MenuOpen then
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


createSelectingRange : Model -> Parts -> PartsRange
createSelectingRange model changedValue =
    case model.selection of
        Single _ ->
            { start = changedValue, end = changedValue }

        Range partsRange ->
            { start = partsRange.start, end = changedValue }

        Unselected ->
            { start = changedValue, end = changedValue }

        Selecting partsRange ->
            { start = partsRange.start, end = getEndOfDayParts changedValue }

        Preset _ ->
            { start = changedValue, end = changedValue }

        Before _ ->
            { start = changedValue, end = changedValue }

        After _ ->
            { start = changedValue, end = changedValue }


cancelShift : Model -> ( Model, Cmd Msg )
cancelShift model =
    case model.selection of
        Selecting partsRange ->
            let
                selection =
                    partsRange
                        |> normalizeSelectingRange
                        |> Range
            in
            R2.withNoCmd
                (withUpdatedSelection
                    selection
                    { model
                        | isShiftDown = False
                    }
                )

        _ ->
            R2.withNoCmd
                { model
                    | isShiftDown = False
                }


onKey : RawKey -> Model -> (Key -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
onKey rawKey model onValidKey =
    case Keyboard.anyKeyOriginal rawKey of
        Just key ->
            onValidKey key

        Nothing ->
            R2.withNoCmd model


updateCalendarRange : Model -> Int -> ( Model, Cmd Msg )
updateCalendarRange model intervalChange =
    R2.withNoCmd
        { model
            | visibleCalendarRange =
                calculateNewCalendarRange model intervalChange model.visibleCalendarRange
        }


addYearsToParts : Int -> Parts -> Parts
addYearsToParts intervalChange parts =
    { parts | year = parts.year + intervalChange }


addMonthsToParts : Int -> Parts -> Parts
addMonthsToParts interval parts =
    parts
        |> partsToPosix Time.utc
        |> TimeExtra.add TimeExtra.Month interval Time.utc
        |> posixToParts Time.utc


addDaysToParts : Int -> Parts -> Parts
addDaysToParts interval parts =
    parts
        |> partsToPosix Time.utc
        |> TimeExtra.add TimeExtra.Day interval Time.utc
        |> posixToParts Time.utc


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
            { parts | year = parts.year + intervalChange }
    in
    case model.calendarType of
        YearCalendar ->
            updateWithIntervalFunc yearChange currentVisibleRange

        ThreeMonthCalendar ->
            updateWithIntervalFunc (addMonthsToParts (intervalChange * 3)) currentVisibleRange

        TwoMonthCalendar ->
            updateWithIntervalFunc (addMonthsToParts intervalChange) currentVisibleRange

        OneMonthCalendar ->
            updateWithIntervalFunc (addMonthsToParts intervalChange) currentVisibleRange


presetsDisplay : Model -> Html Msg
presetsDisplay model =
    let
        shouldShowPresetSelector =
            not (List.isEmpty model.presets) && not model.hidePresets

        openPreset =
            if model.presetMenuVisibility == MenuOpen then
                Attrs.class ""

            else
                Html.Events.onClick <| SetPresetMenuVisibility MenuOpen
    in
    if shouldShowPresetSelector then
        div [ Attrs.class "preset--open--wrapper" ]
            [ button [ Attrs.class "preset--open", openPreset ]
                [ text model.languageConfig.presets, downArrow ]
            , if model.presetMenuVisibility == MenuOpen then
                presetMenu model

              else
                text ""
            ]

    else
        div [] []


presetMenu : Model -> Html Msg
presetMenu model =
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
        [ div [ Attrs.class "preset-menu--close", Html.Events.onClick <| SetPresetMenuVisibility MenuClosed ]
            []
        , div [ Attrs.class "preset-menu--content" ] <|
            List.map
                (\p ->
                    div [ Html.Events.onClick <| SelectPreset p, classList p ]
                        [ text <| presetToDisplayString p model.languageConfig ]
                )
                model.presets
        ]


bottomBar : Model -> Html Msg
bottomBar model =
    let
        content =
            case model.datepickerVisibility of
                AlwaysOpen ->
                    [ button [ Attrs.class "reset", Html.Events.onClick Reset ]
                        [ text model.languageConfig.reset ]
                    ]

                _ ->
                    [ button
                        [ Attrs.id "elm-fancy--daterangepicker--done"
                        , Attrs.class "done"
                        , Html.Events.onClick CloseDatePicker
                        ]
                        [ text model.languageConfig.done ]
                    , button [ Attrs.class "reset", Html.Events.onClick Reset ]
                        [ text model.languageConfig.reset ]
                    ]
    in
    div [ Attrs.class "bottom-bar" ]
        content


leftSelector : Model -> Html Msg
leftSelector =
    mkSelector -1 .start "prev-range-selector" "❮"


rightSelector : Model -> Html Msg
rightSelector =
    mkSelector 1 .end "next-range-selector" "❯"


mkSelector : Int -> (PartsRange -> Parts) -> String -> String -> Model -> Html Msg
mkSelector moveInterval partOfRange class textContent model =
    let
        newRange =
            calculateNewCalendarRange model moveInterval model.visibleCalendarRange

        attrs =
            if dateIsOutOfAllowedRange (partOfRange model.visibleCalendarRange) model.allowedRange then
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

                DateTimeFormat _ ->
                    Attrs.class "clock-selected"
    in
    div
        [ class
        , Html.Events.onClick ToggleFormat
        , Attrs.title model.languageConfig.includeTimeTitle
        ]
        [ text "🕒" ]


topBar : Model -> Html Msg
topBar model =
    let
        allVisibleDatesSelection =
            { start = getFirstDayOfYearParts model.visibleCalendarRange.start, end = getLastDayOfYearParts model.visibleCalendarRange.end }
                |> createSelectionInRange model.allowedRange
                |> Range

        ( fullCalendarSelector, class ) =
            case model.calendarType of
                YearCalendar ->
                    ( div [ Attrs.class "full-calendar-selector", onClick <| SetSelection allVisibleDatesSelection ]
                        [ text <| selectionText model.visibleCalendarRange ]
                    , "top-bar--full"
                    )

                _ ->
                    ( text "", "top-bar--partial" )

        isPreset =
            case model.selection of
                Preset _ ->
                    True

                _ ->
                    False

        maybeClock =
            case model.dateSelectionType of
                DateSelection ->
                    div [] []

                DateRangeSelection ->
                    if model.canChooseTime && not isPreset then
                        clockButton model

                    else
                        div [] []
    in
    div [ Attrs.class class ]
        [ fullCalendarSelector
        , presetsDisplay model
        , calendarInput model
        , maybeClock
        ]


createSelectionInRange : PartsRange -> PartsRange -> PartsRange
createSelectionInRange allowedRange partsRange =
    let
        toComparePosix =
            partsToPosix Time.utc

        updatedSelectionStart =
            if posixToMillis (toComparePosix partsRange.start) < posixToMillis (toComparePosix allowedRange.start) then
                allowedRange.start

            else
                partsRange.start

        updatedSelectionEnd =
            if posixToMillis (toComparePosix partsRange.end) > posixToMillis (toComparePosix allowedRange.end) then
                allowedRange.end

            else
                partsRange.end
    in
    { start = updatedSelectionStart, end = updatedSelectionEnd }


selectionText : PartsRange -> String
selectionText visibleRange =
    String.fromInt visibleRange.start.year


calendarInput : Model -> Html Msg
calendarInput model =
    let
        inputText =
            getUserDateInput model.inputText
    in
    div [ Attrs.class "calendar-input" ]
        [ input
            [ Keyboard.Events.on Keypress [ ( Enter, OnInputFinish ) ]
            , Html.Events.onBlur OnInputFinish
            , Html.Events.onInput OnInputChange
            , Attrs.id "elm-fancy--daterangepicker--input"
            , Attrs.placeholder <| inputPlaceHolder model
            , Attrs.value inputText
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


convertInput : Input -> Model -> ( Selection, Format )
convertInput input model =
    case input of
        SingleInput inputDate ->
            singleInputConversion inputDate model

        RangeInput start end ->
            combineInputToRange start end model.clockStyle

        CustomDate selectedCustomDate ->
            let
                selectedPreset =
                    List.filter (\p -> presetToDisplayString p model.languageConfig == selectedCustomDate) model.presets
            in
            case List.head selectedPreset of
                Just preset ->
                    ( Preset preset, DateFormat )

                Nothing ->
                    ( Unselected, DateFormat )

        BeforeInput inputDate ->
            let
                ( parts, format ) =
                    wildcardInputConversion inputDate .end model.clockStyle
            in
            ( Before <| parts, format )

        AfterInput inputDate ->
            let
                ( parts, format ) =
                    wildcardInputConversion inputDate .start model.clockStyle
            in
            ( After <| parts, format )


wildcardInputConversion : InputDate -> (PartsRange -> Parts) -> ClockStyle -> ( Parts, Format )
wildcardInputConversion inputDate getParts clockStyle =
    case inputDate of
        JustYear year ->
            ( getParts <| yearToPartsRange year, DateFormat )

        JustYearAndMonth yearAndMonth ->
            ( getParts <| yearAndMonthToPartsRange yearAndMonth, DateFormat )

        FullDate dateParts ->
            ( datePartsToParts dateParts, DateFormat )

        FullDateTime dateTimeParts ->
            ( dateTimePartsToParts dateTimeParts, DateTimeFormat clockStyle )


singleInputConversion : InputDate -> Model -> ( Selection, Format )
singleInputConversion inputDate model =
    let
        ( initialSelection, format ) =
            convertInputDate inputDate model.clockStyle

        selection =
            case model.dateSelectionType of
                DateRangeSelection ->
                    case initialSelection of
                        Single posix ->
                            Range <| convertSingleIntoRange posix

                        _ ->
                            initialSelection

                DateSelection ->
                    initialSelection
    in
    ( selection, Maybe.withDefault DateFormat format )


combineInputToRange : InputDate -> InputDate -> ClockStyle -> ( Selection, Format )
combineInputToRange start end clockStyle =
    convertInputDateRange ( start, end ) clockStyle


convertInputDate : InputDate -> ClockStyle -> ( Selection, Maybe Format )
convertInputDate inputDate clockStyle =
    case inputDate of
        JustYear year ->
            ( Range <| yearToPartsRange year, Nothing )

        JustYearAndMonth yearAndMonth ->
            ( Range <| yearAndMonthToPartsRange yearAndMonth, Nothing )

        FullDate dateParts ->
            ( Single (datePartsToParts dateParts), Nothing )

        FullDateTime dateTimeParts ->
            ( Single (dateTimePartsToParts dateTimeParts), Just <| DateTimeFormat clockStyle )


convertInputDateRange : ( InputDate, InputDate ) -> ClockStyle -> ( Selection, Format )
convertInputDateRange ( start, end ) clockStyle =
    case ( start, end ) of
        ( JustYear yearStart, JustYear yearEnd ) ->
            ( Range <| PartsRange (yearToParts yearStart) (yearToParts yearEnd), DateFormat )

        ( JustYearAndMonth yearMonthStart, JustYearAndMonth yearMonthEnd ) ->
            ( Range <| PartsRange (yearAndMonthToParts yearMonthStart) (yearAndMonthToParts yearMonthEnd), DateFormat )

        ( FullDate datePartsStart, FullDate datePartsEnd ) ->
            ( Range <| PartsRange (datePartsToParts datePartsStart) (datePartsToParts datePartsEnd), DateFormat )

        ( FullDateTime dateTimePartsStart, FullDateTime dateTimePartsEnd ) ->
            ( Range <| PartsRange (dateTimePartsToParts dateTimePartsStart) (dateTimePartsToParts dateTimePartsEnd), DateTimeFormat clockStyle )

        _ ->
            ( Unselected, DateFormat )


calendarView : Model -> Html Msg
calendarView model =
    case model.calendarType of
        YearCalendar ->
            yearCalendarView model

        ThreeMonthCalendar ->
            monthlyCalendarView model "monthly-small" 2

        TwoMonthCalendar ->
            monthlyCalendarView model "monthly-large" 1

        OneMonthCalendar ->
            monthlyCalendarView model "monthly-large" 0


yearCalendarView : Model -> Html Msg
yearCalendarView model =
    let
        quarter name startMonth endMonth =
            let
                partsRangeForQuarter =
                    partsRangeForMonths startMonth endMonth model.visibleCalendarRange.start.year

                isOutOfRange =
                    dateIsOutOfAllowedRange partsRangeForQuarter.start model.allowedRange
                        || dateIsOutOfAllowedRange partsRangeForQuarter.end model.allowedRange

                attrs =
                    if isOutOfRange then
                        [ Attrs.class "disabled" ]

                    else
                        [ partsRangeForQuarter
                            |> Range
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
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "year-calendar", Html.Events.onMouseLeave MouseOutsideOfCalendar ]
        [ quarters
        , table []
            [ tbody [ Attrs.class "year" ] <|
                List.map (\m -> monthCalendarView m model) <|
                    getMonthsFromRange
                        0
                        11
                        model.visibleCalendarRange
                        getFirstDayOfYearParts
            ]
        ]


monthlyCalendarView : Model -> String -> Int -> Html Msg
monthlyCalendarView model monthClass endInterval =
    div [ Attrs.id "elm-fancy--daterangepicker-calendar", Attrs.class "month-calendar" ]
        [ table []
            [ tbody [ Attrs.class monthClass ] <|
                List.map (\m -> monthCalendarView m model)
                    (getMonthsFromRange 0 endInterval model.visibleCalendarRange getFirstDayOfMonthStartOfDayParts)
            ]
        ]


getMonthsFromRange : Int -> Int -> PartsRange -> (Parts -> Parts) -> List Parts
getMonthsFromRange start end visibleRange fn =
    List.range start end
        |> List.map
            (\x ->
                fn visibleRange.start
                    |> addMonthsToParts x
            )


partsRangeForMonths : Month -> Month -> Int -> PartsRange
partsRangeForMonths startMonth endMonth currentYear =
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
    { start = start, end = end }


monthCalendarView : Parts -> Model -> Html Msg
monthCalendarView currentMonth model =
    let
        firstOfMonth =
            getFirstDayOfMonthStartOfDayParts currentMonth

        lastOfMonth =
            getLastDayOfMonthEndOfDayParts currentMonth

        range =
            { start = firstOfMonth
            , end = lastOfMonth
            }

        selectionInUtc =
            Range range

        wholeMonthIsOutOfRange =
            dateIsOutOfAllowedRange firstOfMonth model.allowedRange
                && dateIsOutOfAllowedRange lastOfMonth model.allowedRange

        attrs =
            if wholeMonthIsOutOfRange then
                [ Attrs.class "disabled", Attrs.class "month--header" ]

            else
                [ Attrs.class "month--header", onClick <| SetSelection selectionInUtc ]
    in
    td []
        [ table []
            [ thead attrs
                [ text <| monthFormatter model.languageConfig Time.utc (partsToPosix Time.utc currentMonth) ]
            , tbody [ Attrs.class "month" ] <|
                List.map (\x -> dayCalendarView currentMonth x model) <|
                    getCurrentMonthDatesFullWeeks currentMonth
            ]
        ]


dayCalendarView : Parts -> Parts -> Model -> Html Msg
dayCalendarView currentMonth currentDay model =
    let
        wantedMonth =
            currentMonth.month

        contentIsInCorrectMonth =
            currentDay.month == wantedMonth

        today =
            getTodayParts model

        ( hoverAttr, content, setDate ) =
            if contentIsInCorrectMonth then
                if dateIsOutOfAllowedRange currentDay model.allowedRange then
                    ( Attrs.class "", [ text <| String.fromInt currentDay.day ], Attrs.class "" )

                else
                    ( Html.Events.onMouseOver <| OnHoverOverDay currentDay
                    , [ text <| String.fromInt currentDay.day ]
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

        --All local
        ( selectionStart, selectionEnd, isInSelectionRange ) =
            selectionPoints currentDay model

        classList =
            Attrs.classList
                [ ( "day", True )
                , ( "selected-range", contentIsInCorrectMonth && isInSelectionRange )
                , ( "border-selection", isSameDayOfSelection selectionStart || isSameDayOfSelection selectionEnd )
                , ( "today", isSameDay currentDay today )
                , ( "disabled", dateIsOutOfAllowedRange currentDay model.allowedRange )
                , ( "wrong-month", not contentIsInCorrectMonth )
                ]
    in
    td [ classList, setDate, hoverAttr ] [ div [] content ]


calendarPositioning : Maybe Element -> Maybe Element -> WindowSize -> Maybe Int -> List (Attribute msg)
calendarPositioning buttonElement calendarElement windowSize ypadding =
    case ( buttonElement, calendarElement ) of
        ( Just button, Just calendar ) ->
            [ calculateYPosition button calendar windowSize (Maybe.withDefault 0 ypadding)
            , calculateXPosition button calendar windowSize
            ]

        _ ->
            [ Attrs.style "left" "-9999px" ]


checkIfPositioningPossible : Model -> Bool
checkIfPositioningPossible { uiElement, uiButton } =
    case ( uiElement, uiButton ) of
        ( Just _, Just _ ) ->
            True

        _ ->
            False


addPx : String -> String
addPx str =
    str ++ "px"


calculateYPosition : Element -> Element -> WindowSize -> Int -> Attribute msg
calculateYPosition button calendar { height } ypadding =
    let
        ( yNum, yName ) =
            if button.element.y < (height / 2) then
                ( additionalCalcForTop button.element.y + toFloat ypadding, "top" )

            else
                ( additionalCalcForBottom (height - (button.element.y + button.element.height)) - toFloat ypadding, "bottom" )

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


dateIsOutOfAllowedRange : Parts -> PartsRange -> Bool
dateIsOutOfAllowedRange parts { start, end } =
    let
        startPosix =
            partsToPosix Time.utc start

        endPosix =
            partsToPosix Time.utc end
    in
    posixToMillis (partsToPosix Time.utc parts) < posixToMillis startPosix || posixToMillis (partsToPosix Time.utc parts) > posixToMillis endPosix


normalizeSelectingRange : PartsRange -> PartsRange
normalizeSelectingRange ({ start, end } as partsRange) =
    if posixToMillis (partsToPosix Time.utc start) > posixToMillis (partsToPosix Time.utc end) then
        { start = getStartOfDayParts end, end = getEndOfDayParts start }

    else
        partsRange


selectionPoints : Parts -> Model -> ( Maybe Parts, Maybe Parts, Bool )
selectionPoints date ({ selection, allowedRange, displayTimezone } as model) =
    let
        compareRange range =
            (posixToMillis <| partsToPosix displayTimezone range.start)
                <= (posixToMillis <| partsToPosix displayTimezone date)
                && (posixToMillis <| partsToPosix displayTimezone date)
                <= (posixToMillis <| partsToPosix displayTimezone range.end)
    in
    case selection of
        Single parts ->
            ( Just parts
            , Just parts
            , False
            )

        Range partsRange ->
            ( Just partsRange.start
            , Just partsRange.end
            , compareRange partsRange
            )

        Unselected ->
            ( Nothing, Nothing, False )

        Selecting partsRange ->
            let
                normalized =
                    normalizeSelectingRange partsRange
            in
            ( Just normalized.start
            , Just normalized.end
            , compareRange normalized
            )

        Preset presetType ->
            let
                partsRange =
                    presetToPartsRange (DatePicker model) presetType
            in
            ( Just partsRange.start
            , Just partsRange.end
            , compareRange partsRange
            )

        After parts ->
            ( Just parts
            , Just allowedRange.end
            , compareRange { start = parts, end = allowedRange.end }
            )

        Before parts ->
            ( Just allowedRange.start
            , Just parts
            , compareRange { start = allowedRange.start, end = parts }
            )


isSameDay : Parts -> Parts -> Bool
isSameDay parts1 parts2 =
    parts1.day == parts2.day && parts1.month == parts2.month && parts1.year == parts2.year


calcVisibleRange : Parts -> CalendarType -> PartsRange
calcVisibleRange localToday calendarType =
    case calendarType of
        YearCalendar ->
            { start = getFirstDayOfMonthStartOfDayParts { localToday | month = Jan }
            , end = getLastDayOfMonthEndOfDayParts { localToday | month = Dec }
            }

        ThreeMonthCalendar ->
            { start = getFirstDayOfMonthStartOfDayParts { localToday | month = getPrevMonth localToday.month }
            , end = getLastDayOfMonthEndOfDayParts { localToday | month = getNextMonth localToday.month }
            }

        TwoMonthCalendar ->
            { start = getFirstDayOfMonthStartOfDayParts { localToday | month = getPrevMonth localToday.month }
            , end = getLastDayOfMonthEndOfDayParts localToday
            }

        OneMonthCalendar ->
            { start = getFirstDayOfMonthStartOfDayParts localToday
            , end = getLastDayOfMonthEndOfDayParts localToday
            }


convertToRange : Parts -> CalendarType -> PartsRange
convertToRange day calendarType =
    case calendarType of
        YearCalendar ->
            { start = getFirstDayOfYearParts day, end = getLastDayOfYearParts day }

        ThreeMonthCalendar ->
            { start = getFirstDayOfMonthStartOfDayParts { day | month = getPrevMonth day.month }
            , end = getLastDayOfMonthEndOfDayParts { day | month = getNextMonth day.month }
            }

        TwoMonthCalendar ->
            { start = getFirstDayOfMonthStartOfDayParts { day | month = getPrevMonth day.month }
            , end = getLastDayOfMonthEndOfDayParts day
            }

        OneMonthCalendar ->
            { start = getFirstDayOfMonthStartOfDayParts day
            , end = getLastDayOfMonthEndOfDayParts day
            }


getVisibleRangeFromSelection : Selection -> CalendarType -> DatePicker -> PartsRange
getVisibleRangeFromSelection selection calendarType (DatePicker model) =
    case selection of
        Single parts ->
            { start = getStartOfDayParts parts, end = getEndOfDayParts parts }

        Range partsRange ->
            convertToRange partsRange.start calendarType

        Unselected ->
            model.visibleCalendarRange

        Selecting _ ->
            model.visibleCalendarRange

        Preset presetType ->
            presetToPartsRange (DatePicker model) presetType
                |> (\{ start, end } -> PartsRange start end)

        Before parts ->
            { start = getStartOfDayParts parts, end = getEndOfDayParts parts }

        After parts ->
            { start = getStartOfDayParts parts, end = getEndOfDayParts parts }


prettyFormatSelection : DatePicker -> String
prettyFormatSelection (DatePicker model) =
    prettyFormatSelectionInZone (DatePicker model) model.displayTimezone


prettyFormatSelectionInZone : DatePicker -> Zone -> String
prettyFormatSelectionInZone (DatePicker model) displayZone =
    let
        formatSingle parts =
            singleFormatter model.canChooseTime displayZone model.languageConfig model.displayFormat ( parts, model.displayTimezone )

        formatRange range =
            rangeFormatter model.canChooseTime displayZone model.languageConfig model.displayFormat ( range, model.displayTimezone )
    in
    case model.selection of
        Single parts ->
            formatSingle parts

        Range partsRange ->
            if isSameDay partsRange.start partsRange.end && model.displayFormat /= DateTimeFormat model.clockStyle then
                formatSingle partsRange.start

            else
                formatRange partsRange

        Unselected ->
            ""

        Selecting partsRange ->
            if isSameDay partsRange.start partsRange.end then
                formatSingle partsRange.start

            else
                formatRange partsRange

        Preset presetType ->
            presetToDisplayString presetType model.languageConfig

        Before parts ->
            formatSingle parts ++ " " ++ model.languageConfig.beforeThisDate

        After parts ->
            formatSingle parts ++ " " ++ model.languageConfig.afterThisDate


singleFormatter : Bool -> Zone -> LanguageConfig -> Format -> ( Parts, Zone ) -> String
singleFormatter displayTime zone language format ( parts, partsTimezone ) =
    let
        timeParts =
            case format of
                DateFormat ->
                    []

                DateTimeFormat clockStyle ->
                    case clockStyle of
                        H24 ->
                            if displayTime then
                                [ DateFormat.text " "
                                , DateFormat.hourMilitaryFixed
                                , DateFormat.text ":"
                                , DateFormat.minuteFixed
                                ]

                            else
                                []

                        H12 ->
                            if displayTime then
                                [ DateFormat.text " "
                                , DateFormat.hourFixed
                                , DateFormat.text ":"
                                , DateFormat.minuteFixed
                                , DateFormat.text " "
                                , DateFormat.amPmUppercase
                                ]

                            else
                                []
    in
    DateFormat.formatWithLanguage language.dateFormatLanguage
        (language.displayFormat ++ timeParts)
        zone
        (partsToPosix partsTimezone parts)


monthFormatter : LanguageConfig -> Zone -> Posix -> String
monthFormatter language =
    DateFormat.formatWithLanguage language.dateFormatLanguage
        [ DateFormat.monthNameFull
        ]


rangeFormatter : Bool -> Zone -> LanguageConfig -> Format -> ( PartsRange, Zone ) -> String
rangeFormatter canDisplayTime zone language format ( { start, end }, partsZone ) =
    let
        ( beginning, ending ) =
            if posixToMillis (partsToPosix Time.utc start) > posixToMillis (partsToPosix Time.utc end) then
                ( getStartOfDayParts end, getEndOfDayParts start )

            else
                ( start, end )
    in
    singleFormatter canDisplayTime zone language format ( beginning, partsZone )
        ++ " "
        ++ language.to
        ++ " "
        ++ singleFormatter canDisplayTime zone language format ( ending, partsZone )



-- Copied from Derberos.Date.Utils and edited to make sunday first day of the week


getCurrentMonthDatesFullWeeks : Parts -> List Parts
getCurrentMonthDatesFullWeeks time =
    let
        firstDayOfMonth =
            time
                |> getFirstDayOfMonthStartOfDayParts
                |> partsToPosix Time.utc
                |> (\x -> TimeExtra.add TimeExtra.Day (getDiffLastSun (getWeekday Time.utc x)) Time.utc x)

        lastDayOfMonth =
            time
                |> getLastDayOfMonthEndOfDayParts
                |> partsToPosix Time.utc
                |> (\x -> TimeExtra.add TimeExtra.Day (getDiffNextSat (getWeekday Time.utc x)) Time.utc x)

        numberDaysInMonth =
            (posixToMillis lastDayOfMonth - posixToMillis firstDayOfMonth) // (1000 * 60 * 60 * 24)

        -- numberOfDaysInMonth time.year time.month
        addDayParts posix i =
            TimeExtra.add TimeExtra.Day i Time.utc posix
    in
    List.range 0 numberDaysInMonth
        |> List.map (addDayParts firstDayOfMonth)
        |> List.map (posixToParts Time.utc)


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


yearToPartsRange : Int -> PartsRange
yearToPartsRange year =
    let
        parts =
            yearToParts year
    in
    { start = getFirstDayOfYearParts parts
    , end = getLastDayOfYearParts parts
    }


yearAndMonthToPartsRange : YearAndMonth -> PartsRange
yearAndMonthToPartsRange yearMonth =
    let
        parts =
            yearAndMonthToParts yearMonth
    in
    { start = parts
    , end = parts
    }


checkForChange : (DatePicker -> Maybe a) -> DatePicker -> Maybe a -> Bool
checkForChange checkFunc model elementForComparison =
    if isOpen model then
        False

    else
        checkFunc model /= elementForComparison


convertSingleIntoRange : Parts -> PartsRange
convertSingleIntoRange parts =
    let
        timeHasBeenSetByUser =
            parts /= getStartOfDayParts parts
    in
    if timeHasBeenSetByUser then
        { start = parts, end = parts }

    else
        { start = getStartOfDayParts parts, end = getEndOfDayParts parts }



--------------------------------------------------------------------------------------------
--            PRESETS
--------------------------------------------------------------------------------------------


convertInterval : Interval -> Int -> Parts -> Parts
convertInterval interval intervalValue today =
    case interval of
        Days ->
            addDaysToParts intervalValue today

        Months ->
            addMonthsToParts intervalValue today

        Weeks ->
            addDaysToParts (intervalValue * 7) today

        Years ->
            addYearsToParts intervalValue today


presetToPartsRange : DatePicker -> PresetType -> PartsRange
presetToPartsRange (DatePicker model) presetType =
    let
        todayParts =
            posixToParts model.displayTimezone model.now
    in
    case presetType of
        Today ->
            { start = getStartOfDayParts todayParts, end = getEndOfDayParts todayParts }

        Yesterday ->
            { start = getStartOfDayParts (addDaysToParts -1 todayParts), end = getEndOfDayParts (addDaysToParts -1 todayParts) }

        PastWeek ->
            { start = getStartOfDayParts (addDaysToParts -7 todayParts), end = getEndOfDayParts todayParts }

        PastMonth ->
            { start = getStartOfDayParts (addMonthsToParts -1 todayParts), end = getEndOfDayParts todayParts }

        PastYear ->
            { start = getStartOfDayParts (addYearsToParts -1 todayParts), end = getEndOfDayParts todayParts }

        Custom customPreset ->
            { start =
                convertInterval customPreset.intervalStart customPreset.intervalStartValue todayParts
                    |> getStartOfDayParts
            , end =
                convertInterval customPreset.intervalEnd customPreset.intervalEndValue todayParts
                    |> getEndOfDayParts
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



{- conversion from partsrnage to posix range -}


partsRangeToPosixRange : Zone -> PartsRange -> PosixRange
partsRangeToPosixRange zone { start, end } =
    { start = partsToPosix zone start, end = partsToPosix zone end }
