module DateRangePicker
    exposing
        ( Msg
        , DateRangePicker
        , init
        , update
        , defaultSettings
        , isOpen
        )

import Date exposing (Date, Day(..), Month(..), day, month, year)
import Html exposing (Html)
import Html.Attributes as Attrs
import Task
import DateRangePicker.Date exposing (initDate)


{-| An opaque type representing messages that are passed within the DateRangePicker.
-}
type Msg
    = CurrentDate Date
    | SelectDate (Maybe Date)


{-| The settings that the DateRangePicker uses
-}
type alias Settings =
    { placeholder : String
    , className : Maybe String
    , inputName : Maybe String
    , inputId : Maybe String
    , inputAttributes : List (Html.Attribute Msg)
    }


{-| The model to be used within the DateRangePicker.
-}
type alias Model =
    { today : Date
    , startDate : Maybe Date
    , endDate : Maybe Date
    , inputText : Maybe String
    , open : Bool
    }


{-| The DateRangePicker model.
-}
type DateRangePicker
    = DateRangePicker Model


defaultSettings : Settings
defaultSettings =
    { placeholder = "Select a date..."
    , className = Just "elm-daterangepicker"
    , inputName = Nothing
    , inputId = Nothing
    , inputAttributes = []
    }


init : ( DateRangePicker, Cmd Msg )
init =
    ( DateRangePicker <|
        { today = initDate
        , startDate = Nothing
        , endDate = Nothing
        , inputText = Nothing
        , open = False
        }
    , Task.perform CurrentDate Date.now
    )


update : Settings -> Msg -> DateRangePicker -> ( DateRangePicker, Cmd Msg )
update settings msg (DateRangePicker model) =
    case msg of
        CurrentDate date ->
            { model | today = date } ! []

        _ ->
            model ! []


{-| Expose if the daterangepicker is open
-}
isOpen : DateRangePicker -> Bool
isOpen (DateRangePicker model) =
    model.open


(!) : Model -> List (Cmd Msg) -> ( DateRangePicker, Cmd Msg )
(!) model cmds =
    ( DateRangePicker model, Cmd.batch cmds )
