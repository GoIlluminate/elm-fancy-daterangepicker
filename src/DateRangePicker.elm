module DateRangePicker
    exposing
        ( DateRangePicker
        , Model
        , Settings
        , Msg
        , defaultSettings
        , update
        , init
        )

import Html exposing (Html, Attribute)
import Html.Attributes exposing (style, class)
import Html.Events exposing (on)
import Date

{-| The base model for the datepicker
-}
type DateRangePicker
    = DateRangePicker Model

{-| Represents your datepicker as a model
-}
type alias Model =
    { settings : Settings
    }

type alias Settings =
    { dateRange : Bool
    }

{-| Settings for your datepicker
# startDate = start date
# endDate = end date
# minDate = earliest date the datepicker goes to
# maxDate = latest date the datepicker goes to
# singleDatePicker = option for selecting only a single date or a daterange
-}
type alias PossibleSettings =
    { startDate : Date.Date
    , endDate : Date.Date
    , minDate : Date.Date
    , maxDate : Date.Date
    , singleDatePicker : Bool
    }

defaultSettings : Settings
defaultSettings =
    { dateRange = True
    }

type Msg
    = NoOp

init : DateRangePicker
init =
    let
        model =
            { settings = defaultSettings
            }
    in
        DateRangePicker model

update : DateRangePicker -> Msg -> (DateRangePicker, Cmd Msg)
update (DateRangePicker model) msg =
    case msg of
        NoOp ->
            (DateRangePicker model, Cmd.none)