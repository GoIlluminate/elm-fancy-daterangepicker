module DateRangePicker.Helper exposing (..)

import Html
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


{-| An opaque function that prevents default click events.
-}
onDragNoDefault : msg -> Html.Attribute msg
onDragNoDefault message =
    Events.custom "drag" <|
        Json.succeed
            { message = message
            , stopPropagation = False
            , preventDefault = True
            }
