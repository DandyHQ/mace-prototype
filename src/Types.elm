module Types exposing (Window(..), Frame(..), FrameChildren(..), Tile(..))

-- Window ID
type Window = Window Int

-- Frame Width Height TileStyle (List Children)
type Frame = Frame Int Int Tile FrameChildren

type FrameChildren = FrameFrame (List Frame) | WindowFrame (List Window)

type Tile = Horiz | Vert | None
