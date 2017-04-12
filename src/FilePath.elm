module FilePath exposing (takeFileName, takeDirectory, takeExtension, dropExtension, takeBaseName)

import Maybe

takeFileName : String -> String
takeFileName path =
  Maybe.withDefault "" <| List.head <| List.reverse <| String.split "/" path

takeDirectory : String -> String
takeDirectory path =
  String.join "/" <| List.reverse <| List.drop 1 <| List.reverse <| String.split "/" path

takeExtension : String -> String
takeExtension path =
  Maybe.withDefault "" <| List.head <| List.reverse <| String.split "." path

dropExtension : String -> String
dropExtension path =
  String.join "." <| List.reverse <| List.drop 1 <| List.reverse <| String.split "." path

takeBaseName : String -> String
takeBaseName path =
  takeFileName path |> dropExtension
