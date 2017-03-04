import Html exposing (program)
import Model exposing (Model)
import Msg exposing (Msg)
import Update
import View


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
  Sub.none
