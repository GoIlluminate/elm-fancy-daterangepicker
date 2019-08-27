module DateRangePicker.Helper exposing (mouseDownNoDefault, onClickNoDefault)

import Html exposing (Html)
import Html.Events as Events
import Json.Decode as Json


{-| An opaque function that prevents default click events.
-}
onClickNoDefault : msg -> Html.Attribute msg
onClickNoDefault message =
    Events.custom "click" <|
        Json.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = True
            }


{-| An opaque function that prevents default click events.
-}
mouseDownNoDefault : msg -> Html.Attribute msg
mouseDownNoDefault message =
    Events.custom "mousedown" <|
        Json.succeed
            { message = message
            , stopPropagation = False
            , preventDefault = True
            }
