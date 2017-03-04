module Types exposing (Frame(..), Tile(..))

type Frame = Frame Tile (List Frame) | Window Int

type Tile = Horiz | Vert | Tabbed Int
