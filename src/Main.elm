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
  case model.resizeDrag of
    Nothing ->
      Window.resizes WindowResize

    Just _ ->
      Sub.batch
        [ Mouse.moves ResizeAt
        , Mouse.ups ResizeEnd
        , Window.resizes WindowResize
        ]
