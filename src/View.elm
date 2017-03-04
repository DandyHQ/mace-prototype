module View exposing (view)

import Array
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Msg exposing (Msg)
import Types exposing (Frame(..), Tile(..))

(=>) =
  (,)

view : Model -> Html Msg
view model =
  frame 100 100 model.frames

frame w h model =
  case model of
    Frame Horiz children ->
      div [ style [ "display" => "flex", "flex-flow" => "column", "width" => (toString w ++ "%"), "height" => (toString h ++ "%") ] ] (List.map (frame 100 (100 // (List.length children))) children)
    Frame Vert children ->
      div [ style [ "display" => "flex", "flex-flow" => "row", "width" => (toString w ++ "%"), "height" => (toString h ++ "%") ] ] (List.map (frame (100 // (List.length children)) 100) children)
    Frame (Tabbed focused) children ->
      div [ style [ "width" => (toString w ++ "%"), "height" => (toString h ++ "%") ] ]
        [ div []
            (List.indexedMap (\k v -> button [ onClick (Msg.FocusTab v) ] [ text ("tab " ++ toString k) ]) children)
        , case Array.get focused (Array.fromList children) of
            Just a -> frame 100 100 a
            Nothing -> text "Error occured"
        ]
    Window id ->
      div [ style [ "display" => "flex", "flex-flow" => "column", "width" => (toString w ++ "%"), "height" => (toString h ++ "%"), "border" => "1px solid #aaa" ] ]
        [ div [ style [ "flex" => "0 1 auto" ] ]
            [ text (toString id)
            , button [ onClick (Msg.NewWindow id Vert) ] [ text "new-v" ]
            , button [ onClick (Msg.NewWindow id Horiz) ] [ text "new-h" ]
            , button [ onClick (Msg.NewWindow id (Tabbed 0)) ] [ text "new-tab" ]
            ]
        , textarea [ style [ "flex" => "1 1 auto" ] ] []
        ]
