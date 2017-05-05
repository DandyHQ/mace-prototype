import Html exposing (program)
import Model exposing (Model)
import Mouse
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
  Sub.batch
    ([Window.resizes WindowResize]
      ++
      if model.resizeDrag /= Nothing || model.moveDrag /= Nothing then
        [ Mouse.moves DragAt
        , Mouse.ups DragEnd
        ]
      else []
    )
