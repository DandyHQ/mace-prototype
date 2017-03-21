import Html exposing (program)
import Model exposing (Model)
import Msg exposing (..)
import Update
import View
import Window


main =
  Html.program
    { init = Model.init
    , view = View.view
    , update = Update.update
    , subscriptions = subscriptions
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Window.resizes WindowResize
