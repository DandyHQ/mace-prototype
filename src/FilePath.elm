module FilePath exposing (takeFileName, takeDirectory, takeExtension, dropExtension, takeBaseName)

import Maybe

takeFileName : String -> String
takeFileName path =
  Maybe.withDefault "" <| List.head <| List.reverse <| String.split "/" path

takeDirectory : String -> String
takeDirectory path =
  let leading = if String.left 1 path == "/" then "/" else "" in
  String.split "/" path
    |> List.reverse
    |> List.drop 1
    |> List.reverse
    |> List.filter ((/=) "")
    |> String.join "/"
    |> (++) leading

takeExtension : String -> String
takeExtension path =
  takeFileName path
    |> String.split "."
    |> List.tail
    |> Maybe.withDefault []
    |> String.join "."

dropExtension : String -> String
dropExtension path =
  let extLength = String.length (takeExtension path) in
  String.dropRight (extLength + if extLength /= 0 then 1 else 0) path

takeBaseName : String -> String
takeBaseName path =
  takeFileName path |> dropExtension
