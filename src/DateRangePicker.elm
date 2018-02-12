module DateRangePicker
    exposing
        ( Msg
        , DateRangePicker
        , init
        , update
        , defaultSettings
        , isOpen
        , view
        )

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, text, table, thead, th, tbody, tr, td, p, h1, input, button, a)
import Html.Attributes as Attrs exposing (class, colspan, type_, placeholder, value, href)
import Html.Events exposing (onClick, on, onBlur, onInput, onFocus, onWithOptions)
import Task
import List.Extra as LE
import DateRangePicker.Date exposing (initDate, mkDate, startOfMonth, endOfMonth, datesInRangeIncl, dayToInt, dayFromInt, formatDay, formatDate, formatMonth, daysInMonth)
import Json.Decode as Json


{-| An opaque type representing messages that are passed within the DateRangePicker.
-}
type Msg
    = CurrentDate Date
    | PrevYear
    | NextYear
    | SetDateRange DateRange
    | SetDate Date
    | DoNothing
    | Focus
    | Blur
    | MouseDown
    | MouseUp
    | Close
    | Reset


type alias DateRange =
    { start : Date
    , end : Date
    }


{-| The settings that the DateRangePicker uses
-}
type alias Settings =
    { placeholder : String
    , classNamespace : String
    , inputClassList : List ( String, Bool )
    , inputName : Maybe String
    , inputId : Maybe String
    , inputAttributes : List (Html.Attribute Msg)
    }


{-| The model to be used within the DateRangePicker.
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
    }


{-| The DateRangePicker model.
-}
type DateRangePicker
    = DateRangePicker Model


type alias Quarter =
    { name : String
    , months : List (List Date)
    }


type alias FullYear =
    { name : String
    , year : Int
    , quarters : List Quarter
    }


defaultSettings : Settings
defaultSettings =
    { placeholder = "Select a date..."
    , classNamespace = "elm-daterangepicker"
    , inputClassList = []
    , inputName = Nothing
    , inputId = Nothing
    , inputAttributes = []
    }


init : ( DateRangePicker, Cmd Msg )
init =
    ( DateRangePicker initModel
    , initCmd
    )


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
    }


initCmd : Cmd Msg
initCmd =
    Task.perform CurrentDate Date.now


update : Settings -> Msg -> DateRangePicker -> ( DateRangePicker, Cmd Msg )
update settings msg (DateRangePicker ({ forceOpen } as model)) =
    let
        ( newModel, cmds ) =
            case msg of
                CurrentDate date ->
                    { model | today = date, currentYear = prepareYear date } ! []

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

                SetDateRange dateRange ->
                    { model | dateRange = Just dateRange } ! []

                SetDate date ->
                    case ( model.startDate, model.endDate ) of
                        ( Just a, Just b ) ->
                            { model
                                | startDate = Just date
                                , endDate = Nothing
                                , dateRange = Nothing
                            }
                                ! []

                        ( Just a, Nothing ) ->
                            let
                                start =
                                    model.startDate

                                end =
                                    Just date

                                dateRange =
                                    case ( start, end ) of
                                        ( Just aa, Just bb ) ->
                                            if aa !<= bb then
                                                Just
                                                    { start = aa
                                                    , end = bb
                                                    }
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
                                        }
                                            ! []

                                    Nothing ->
                                        { model
                                            | startDate = Just date
                                            , endDate = Nothing
                                            , dateRange = Nothing
                                        }
                                            ! []

                        ( Nothing, Nothing ) ->
                            { model
                                | startDate = Just date
                                , dateRange = Nothing
                            }
                                ! []

                        ( _, _ ) ->
                            model ! []

                Focus ->
                    { model | open = True, forceOpen = False } ! []

                Blur ->
                    { model | open = forceOpen } ! []

                MouseDown ->
                    { model | forceOpen = True } ! []

                MouseUp ->
                    { model | forceOpen = False } ! []

                Close ->
                    { model | open = False, forceOpen = False } ! []

                Reset ->
                    initModel ! [ initCmd ]

                DoNothing ->
                    model ! []
    in
        (updateInputText newModel) !> [ cmds ]


{-| Expose if the daterange picker is open
-}
isOpen : DateRangePicker -> Bool
isOpen (DateRangePicker model) =
    model.open


mkClass : Settings -> String -> Html.Attribute msg
mkClass { classNamespace } c =
    Attrs.class (classNamespace ++ c)


{-| The daterange picker view. The date range passed is whatever date range it should treat as selected.
-}
view : ( Maybe Date, Maybe Date ) -> Settings -> DateRangePicker -> Html Msg
view ( selectedStartDate, selectedEndDate ) settings (DateRangePicker ({ open } as model)) =
    let
        class =
            mkClass settings

        potentialInputId =
            settings.inputId
                |> Maybe.map Attrs.id
                |> (List.singleton >> List.filterMap identity)

        inputClasses =
            [ ( settings.classNamespace ++ "input", True ) ]
                ++ settings.inputClassList

        inputCommon xs =
            input
                ([ Attrs.classList inputClasses
                 , Attrs.name (settings.inputName ?> "")
                 , type_ "text"
                 , onBlur Blur
                 , onClick Focus
                 , onFocus Focus
                 ]
                    ++ settings.inputAttributes
                    ++ potentialInputId
                    ++ xs
                )
                []

        dateInput =
            inputCommon
                [ placeholder settings.placeholder
                , model.inputText
                    |> Maybe.withDefault
                        settings.placeholder
                    |> value
                ]
    in
        div [ class "daterangepicker-container" ]
            [ dateInput
            , if open then
                dateRangePicker model
              else
                text ""
            ]


dateRangePicker : Model -> Html Msg
dateRangePicker model =
    let
        onPicker ev =
            Json.succeed
                >> onWithOptions ev
                    { preventDefault = False
                    , stopPropagation = True
                    }
    in
        div
            [ class "full-year-calendar-wrapper"
            , onPicker "mousedown" MouseDown
            , onPicker "mouseup" MouseUp
            ]
            [ div [ class "full-year-calendar" ] <|
                (printYearLabel model.currentYear
                    ++ printQuarters model
                    ++ printFooter
                )
            ]


printFooter : List (Html Msg)
printFooter =
    [ div [ class "daterangepicker-footer" ]
        [ button [ onClick Close, class "done-btn" ] [ text "Done" ]
        , button [ onClick Reset, class "reset-btn" ] [ text "Reset" ]
        ]
    ]


printYearLabel : FullYear -> List (Html Msg)
printYearLabel fullYear =
    let
        start =
            mkDate fullYear.year Jan 1

        end =
            mkDate fullYear.year Dec 31

        setYearRange =
            onClick <|
                SetDateRange <|
                    { start = start
                    , end = end
                    }
    in
        [ div [ class "yr-label-wrapper" ]
            [ div [ class "yr-btn yr-prev", onClick PrevYear ] []
            , div [ class "yr-btn yr-label", setYearRange ] [ text fullYear.name ]
            , div [ class "yr-btn yr-next", onClick NextYear ] []
            ]
        ]


printQuarters : Model -> List (Html Msg)
printQuarters model =
    let
        quarters =
            model.currentYear.quarters
    in
        List.map (printQuarter model) quarters


printQuarter : Model -> Quarter -> Html Msg
printQuarter model qtr =
    let
        m1 =
            List.head qtr.months

        m2 =
            List.head <|
                List.reverse qtr.months
    in
        case ( m1, m2 ) of
            ( Just a, Just b ) ->
                let
                    startOfQuarter =
                        List.head a

                    endOfQuarter =
                        List.head <|
                            List.reverse b

                    setQuarterDateRange =
                        case ( startOfQuarter, endOfQuarter ) of
                            ( Just aa, Just bb ) ->
                                onClick <|
                                    SetDateRange <|
                                        { start = aa
                                        , end = bb
                                        }

                            ( _, _ ) ->
                                onClick DoNothing

                    monthDiv =
                        div [ class "qtr-row" ] <|
                            ([ div [ class "qtr-label", setQuarterDateRange ] [ text qtr.name ] ]
                                ++ List.map (printMonth model) qtr.months
                            )
                in
                    monthDiv

            ( _, _ ) ->
                div [] []


printMonth : Model -> List Date -> Html Msg
printMonth model m =
    let
        h =
            List.head m
    in
        case h of
            Just a ->
                let
                    days =
                        [] ++ padDaysLeft a ++ List.map (printDay model) m

                    setMonthDateRange =
                        onClick <|
                            SetDateRange <|
                                { start = startOfMonth a
                                , end = endOfMonth a
                                }

                    monthDiv =
                        div [ class "month-label", setMonthDateRange ]
                            [ text <|
                                formatMonth <|
                                    month a
                            ]
                in
                    div [ class "month" ]
                        ([ monthDiv ]
                            ++ printDaysOfWeek
                            ++ days
                            ++ padMonth (42 - List.length days)
                        )

            _ ->
                div [] []


printDaysOfWeek : List (Html Msg)
printDaysOfWeek =
    let
        days =
            List.range 1 7

        go n =
            div [ class "dow" ]
                [ text <|
                    formatDay <|
                        dayFromInt n
                ]
    in
        List.map go days


padDaysLeft : Date -> List (Html Msg)
padDaysLeft d =
    let
        dd =
            dayToInt <| dayOfWeek d

        go =
            div [ class "day-filler" ] []
    in
        List.repeat (dd - 1) go


padDaysRight : Date -> List (Html Msg)
padDaysRight d =
    let
        dd =
            dayToInt <| dayOfWeek d

        go =
            div [ class "day-filler" ] []
    in
        List.repeat (7 - dd) go


padMonth : Int -> List (Html Msg)
padMonth i =
    List.repeat i <| div [ class "day-filler" ] []


printDay : Model -> Date -> Html Msg
printDay model date =
    let
        className =
            case model.dateRange of
                Just a ->
                    if (inRange date a) then
                        "day selected-range"
                    else
                        "day"

                Nothing ->
                    if isStartOrEnd date model then
                        "day selected-range"
                    else
                        "day"

        setDate =
            onClick <| SetDate date
    in
        div [ class className, setDate ]
            [ text <|
                toString <|
                    day date
            ]


prepareYear : Date -> FullYear
prepareYear date =
    let
        yr =
            year date

        start =
            mkDate yr Jan 1

        end =
            mkDate yr Dec 31

        dates =
            datesInRangeIncl start end
    in
        { name = toString yr
        , year = yr
        , quarters =
            prepareQuarters <|
                LE.groupWhile (\x y -> (month x) == (month y)) dates
        }


chunksOfLeft : Int -> List a -> List (List a)
chunksOfLeft k xs =
    let
        len =
            List.length xs
    in
        if len > k then
            List.take k xs :: chunksOfLeft k (List.drop k xs)
        else
            [ xs ]


prepareQuarters : List (List Date) -> List Quarter
prepareQuarters lst =
    let
        qs =
            chunksOfLeft 3 lst
    in
        List.indexedMap (\idx q -> { name = "Q" ++ (toString (idx + 1)), months = q }) qs


(!) : Model -> List (Cmd Msg) -> ( Model, Cmd Msg )
(!) model cmds =
    ( model, Cmd.batch cmds )


(!>) : Model -> List (Cmd Msg) -> ( DateRangePicker, Cmd Msg )
(!>) model cmds =
    ( DateRangePicker model, Cmd.batch cmds )


inRange : Date -> DateRange -> Bool
inRange date { start, end } =
    let
        ( timeDate, timeStart, timeEnd ) =
            ( Date.toTime date
            , Date.toTime start
            , Date.toTime end
            )
    in
        if timeStart <= timeDate && timeEnd >= timeDate then
            True
        else
            False


isStartOrEnd : Date -> Model -> Bool
isStartOrEnd date model =
    case ( model.startDate, model.endDate ) of
        ( Just a, Just b ) ->
            Date.toTime a == Date.toTime date || Date.toTime b == Date.toTime date

        ( Just a, _ ) ->
            Date.toTime a == Date.toTime date

        ( _, Just b ) ->
            Date.toTime b == Date.toTime date

        ( _, _ ) ->
            False


(!<=) : Date -> Date -> Bool
(!<=) a b =
    Date.toTime a <= Date.toTime b


updateInputText : Model -> Model
updateInputText model =
    case model.dateRange of
        Just a ->
            let
                dateRangeString =
                    String.concat
                        [ formatDate a.start
                        , " - "
                        , formatDate a.end
                        ]
            in
                { model | inputText = Just dateRangeString }

        Nothing ->
            { model | inputText = Nothing }


(?>) : Maybe a -> a -> a
(?>) =
    flip Maybe.withDefault
