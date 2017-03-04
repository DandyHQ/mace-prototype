module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Model exposing (Model)
import Msg exposing (Msg)

(=>) =
  (,)

view : Model -> Html Msg
view model =
  div [ style [ "display" => "flex", "flex-flow" => "column", "height" => "100%" ] ]
    [ div [ style [ "flex" => "0 1 auto" ] ] [ text "header" ]
    , textarea [ style [ "flex" => "1 1 auto" ] ] []
    ]
