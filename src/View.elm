module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Msg exposing (Msg)

(=>) =
  (,)

view : Model -> Html Msg
view model =
  frame 100 100 model.frames

frame w h model =
  case model of
    Model.Frame children ->
      div [ style [ "height" => (toString h ++ "%") ] ] (List.map (frame w (100 // (List.length children))) children)
    Model.Window id ->
      div [ style [ "display" => "flex", "flex-flow" => "column", "height" => (toString h ++ "%") ] ]
        [ div [ style [ "flex" => "0 1 auto" ] ]
            [ text (toString id)
            , button [ onClick (Msg.NewWindow id) ] [ text "new" ]
            , button [] [ text "del" ]
            ]
        , textarea [ style [ "flex" => "1 1 auto" ] ] []
        ]
