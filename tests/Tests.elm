module Tests exposing (..)

import Test exposing (..)
import Types exposing (..)
import Expect
import FilePath
import Frame
import Fuzz exposing (list, int, tuple, string)
import String


all : Test
all =
    describe "mace"
        [ filePath
        , resizeFrame
        , rearrangeFrame
        ]

initFrame : Frame
initFrame =
  Frame "0" (Size 600 600) (Position 0 0) Horiz ( FrameFrame
    [ Frame "00" (Size 300 600) (Position 0 0) Horiz ( WindowFrame (Window NoShadow Nothing 0
        [ Tab "000" "/root/tutorial2.py" "cat"
        , Tab "001" "/root/example2.py" "dog"
        ]))
    , Frame "00" (Size 299 600) (Position 301 0) Horiz ( FrameFrame
        [ Frame "000" (Size 299 300) (Position 301 0) Vert
            ( WindowFrame (Window NoShadow Nothing 0 [ Tab "0000" "/root/readme.md" "tiger" ] ))
        , Frame "001" (Size 299 299) (Position 301 301) Vert ( WindowFrame (Window NoShadow Nothing 0
            [ Tab "0010" "/root/mouse.c" "pidgin"
            , Tab "0011" "/root/example2.py" "frog"
            , Tab "0012" "/root/music.c" "song"
            ]))
        ])
    ])

{-| frame layout after it has been resized to (Size 1539 912) -}
afterResizeAll : Frame
afterResizeAll =
  Frame "0" (Size 1539 912) (Position 0 0) Horiz ( FrameFrame
    [ Frame "00" (Size 770 912) (Position 0 0) Horiz ( WindowFrame (Window NoShadow Nothing 0
        [ Tab "000" "/root/tutorial2.py" "cat"
        , Tab "001" "/root/example2.py" "dog"
        ]))
    , Frame "00" (Size 768 912) (Position 771 0) Horiz ( FrameFrame
        [ Frame "000" (Size 768 456) (Position 771 0) Vert
            ( WindowFrame (Window NoShadow Nothing 0 [ Tab "0000" "/root/readme.md" "tiger" ] ))
        , Frame "001" (Size 768 455) (Position 771 457) Vert ( WindowFrame (Window NoShadow Nothing 0
            [ Tab "0010" "/root/mouse.c" "pidgin"
            , Tab "0011" "/root/example2.py" "frog"
            , Tab "0012" "/root/music.c" "song"
            ]))
        ])
    ])

{-| frame layout after center split has been resized by 100 px -}
afterResize : Frame
afterResize =
  Frame "0" (Size 600 600) (Position 0 0) Horiz ( FrameFrame
    [ Frame "00" (Size 200 600) (Position 0 0) Horiz ( WindowFrame (Window NoShadow Nothing 0
        [ Tab "000" "/root/tutorial2.py" "cat"
        , Tab "001" "/root/example2.py" "dog"
        ]))
    , Frame "00" (Size 399 600) (Position 201 0) Horiz ( FrameFrame
        [ Frame "000" (Size 399 300) (Position 201 0) Vert
            ( WindowFrame (Window NoShadow Nothing 0 [ Tab "0000" "/root/readme.md" "tiger" ] ))
        , Frame "001" (Size 399 299) (Position 201 301) Vert ( WindowFrame (Window NoShadow Nothing 0
            [ Tab "0010" "/root/mouse.c" "pidgin"
            , Tab "0011" "/root/example2.py" "frog"
            , Tab "0012" "/root/music.c" "song"
            ]))
        ])
    ])

resizeDrag : Maybe ResizeDrag
resizeDrag =
  Just (ResizeDrag
    (Frame "00" (Size 300 600) (Position 0 0) Horiz ( WindowFrame (Window NoShadow Nothing 0
        [ Tab "000" "/root/tutorial2.py" "cat"
        , Tab "001" "/root/example2.py" "dog"
        ])))
    (Position 300 300)
    (Position 200 350)
    )

-- this should be replaced with fuzzing
resizeFrame : Test
resizeFrame =
  describe "Resize Frame"
    [ test "perform a resize" <|
        \() ->
          Expect.equal afterResizeAll (Frame.resizeAll (Size 1539 912) initFrame)
    , test "double resize" <|
        \() ->
          Expect.equal initFrame (Frame.resizeAll (Size 600 600) (Frame.resizeAll (Size 1539 912) initFrame))
    , test "resize one frame" <|
        \() ->
          Expect.equal afterResize (Frame.resize resizeDrag initFrame)
    ]

simpleMoveDrag : Maybe MoveDrag
simpleMoveDrag =
  Just (MoveDrag
    (Tab "000" "/root/tutorial2.py" "cat")
    True
    (Position -81 -17)
    (Position 81 17)
    (Position 580 318)
    )

moveLastTab : Maybe MoveDrag
moveLastTab =
  Just (MoveDrag
    (Tab "0000" "/root/readme.md" "tiger")
    True
    (Position -84 -15)
    (Position 385 15)
    (Position 450 315)
    )

afterSimpleRearrange : Frame
afterSimpleRearrange =
  Frame "0" (Size 600 600) (Position 0 0) Horiz ( FrameFrame
    [ Frame "00" (Size 300 600) (Position 0 0) Horiz
      ( WindowFrame (Window NoShadow Nothing 0 [ Tab "000" "/root/example2.py" "dog" ] ))
    , Frame "01" (Size 299 600) (Position 301 0) Horiz ( FrameFrame
        [ Frame "010" (Size 299 300) (Position 301 0) Vert
            ( WindowFrame (Window NoShadow Nothing 0 [ Tab "0100" "/root/readme.md" "tiger" ] ))
        , Frame "011" (Size 299 299) (Position 301 301) Vert ( WindowFrame (Window NoShadow Nothing 3
            [ Tab "0110" "/root/mouse.c" "pidgin"
            , Tab "0111" "/root/example2.py" "frog"
            , Tab "0112" "/root/music.c" "song"
            , Tab "0113" "/root/tutorial2.py" "cat"
            ]))
        ])
    ])

afterMoveLastTab : Frame
afterMoveLastTab =
  Frame "0" (Size 600 600) (Position 0 0) Horiz ( FrameFrame
    [ Frame "00" (Size 300 600) (Position 0 0) Horiz ( WindowFrame (Window NoShadow Nothing 0
        [ Tab "000" "/root/tutorial2.py" "cat"
        , Tab "001" "/root/example2.py" "dog"
        ]))
    , Frame "01" (Size 299 600) (Position 301 0) Horiz ( WindowFrame (Window NoShadow Nothing 1
        [ Tab "010" "/root/mouse.c" "pidgin"
        , Tab "011" "/root/readme.md" "tiger"
        , Tab "012" "/root/example2.py" "frog"
        , Tab "013" "/root/music.c" "song"
        ]))
    ])

rearrangeFrame : Test
rearrangeFrame =
  describe "Rearrange the frames"
    [ test "one tab to another window" <|
        \() ->
          Expect.equal afterSimpleRearrange (Frame.rearrange simpleMoveDrag initFrame)
    , test "moving the last tab out" <|
        \() ->
          Expect.equal afterMoveLastTab (Frame.rearrange moveLastTab initFrame)
    ]

filePath : Test
filePath =
  describe "File and Path Name Manipulation"
    [ describe "Take file name"
        [ test "simple path" <|
            \() ->
                Expect.equal "filename" (FilePath.takeFileName "/root/filename")
        , test "no directory" <|
            \() ->
                Expect.equal "filename" (FilePath.takeFileName "filename")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.takeFileName "")
        , test "root" <|
            \() ->
                Expect.equal "" (FilePath.takeFileName "/")
        , test "file in root" <|
            \() ->
                Expect.equal "filename" (FilePath.takeFileName "/filename")
        ]
    , describe "Take directory"
        [ test "simple path" <|
            \() ->
                Expect.equal "/root" (FilePath.takeDirectory "/root/filename")
        , test "multiple slashes" <|
            \() ->
                Expect.equal "/usr/local" (FilePath.takeDirectory "/usr//local///file")
        , test "no directory" <|
            \() ->
                Expect.equal "" (FilePath.takeDirectory "filename")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.takeDirectory "")
        , test "root" <|
            \() ->
                Expect.equal "/" (FilePath.takeDirectory "/")
        , test "file in root" <|
            \() ->
                Expect.equal "/" (FilePath.takeDirectory "/filename")
        ]
    , describe "Take extension"
        [ test "no extension" <|
            \() ->
                Expect.equal "" (FilePath.takeExtension "/root/filename")
        , test "full stop" <|
            \() ->
                Expect.equal "" (FilePath.takeExtension "/root/filename.")
        , test "simple extension" <|
            \() ->
                Expect.equal "exe" (FilePath.takeExtension "/root/filename.exe")
        ,test "two part extension" <|
            \() ->
                Expect.equal "tar.gz" (FilePath.takeExtension "/root/filename.tar.gz")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.takeExtension "")
        , test "root" <|
            \() ->
                Expect.equal "" (FilePath.takeExtension "/")
        , test "weird pattern" <|
            \() ->
                Expect.equal "nobody.home" (FilePath.takeExtension "/./.nobody.home")
        ]
    , describe "Drop extension"
        [ test "no extension" <|
            \() ->
                Expect.equal "/root/filename" (FilePath.dropExtension "/root/filename")
        , test "full stop" <|
            \() ->
                Expect.equal "/root/filename." (FilePath.dropExtension "/root/filename.")
        , test "simple extension" <|
            \() ->
                Expect.equal "/root/filename" (FilePath.dropExtension "/root/filename.exe")
        ,test "two part extension" <|
            \() ->
                Expect.equal "/root/filename" (FilePath.dropExtension "/root/filename.tar.gz")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.dropExtension "")
        , test "root" <|
            \() ->
                Expect.equal "/" (FilePath.dropExtension "/")
        , test "weird pattern" <|
            \() ->
                Expect.equal "/./" (FilePath.dropExtension "/./.nobody.home")
        ]
    , describe "Take base name"
        [ test "simple path" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "/root/filename")
        , test "no directory" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "filename")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.takeBaseName "")
        , test "root" <|
            \() ->
                Expect.equal "" (FilePath.takeBaseName "/")
        , test "file in root" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "/filename")
        , test "full stop" <|
            \() ->
                Expect.equal "filename." (FilePath.takeBaseName "/root/filename.")
        , test "simple extension" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "/root/filename.exe")
        , test "two part extension" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "/root/filename.tar.gz")
        , test "weird pattern" <|
            \() ->
                Expect.equal "" (FilePath.takeBaseName "/./.nobody.home")
        ]
    ]
