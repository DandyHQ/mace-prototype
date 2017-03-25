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
  case model.drag of
    Nothing ->
      Window.resizes WindowResize

    Just _ ->
      Sub.batch
        [ Mouse.moves DragAt
        , Mouse.ups DragEnd
        , Window.resizes WindowResize
        ]
