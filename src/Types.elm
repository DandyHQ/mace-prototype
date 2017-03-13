module Types exposing (Window(..), Frame(..), FrameChildren(..), Tile(..))

type Window = Window Int

type Frame = Frame Tile FrameChildren

type FrameChildren = FrameFrame (List Frame) | WindowFrame (List Window)

type Tile = Horiz | Vert | None
