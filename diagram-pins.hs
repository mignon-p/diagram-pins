import Codec.Picture( PixelRGBA8( .. ), writePng )
import Control.Monad
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations

wpiPins =
  [ Just 17
  , Just 18
  , Just 27
  , Just 22
  , Just 23
  , Just 24
  , Just 25
  , Just 4
  , Just 2
  , Just 3
  , Just 8
  , Just 7
  , Just 10
  , Just 9
  , Just 11
  , Just 14
  , Just 15
  , Just 28
  , Just 29
  , Just 30
  , Just 31
  , Just 5
  , Just 6
  , Just 13
  , Just 19
  , Just 26
  , Just 12
  , Just 16
  , Just 20
  , Just 21
  , Just 0
  , Just 1
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  ]

physPins =
  [ Nothing
  , Nothing
  , Nothing
  , Just 2
  , Nothing
  , Just 3
  , Nothing
  , Just 4
  , Just 14
  , Nothing
  , Just 15
  , Just 17
  , Just 18
  , Just 27
  , Nothing
  , Just 22
  , Just 23
  , Nothing
  , Just 24
  , Just 10
  , Nothing
  , Just 9
  , Just 25
  , Just 11
  , Just 8
  , Nothing
  , Just 7
  , Just 0
  , Just 1
  , Just 5
  , Nothing
  , Just 6
  , Just 12
  , Just 13
  , Nothing
  , Just 19
  , Just 16
  , Just 26
  , Just 20
  , Nothing
  , Just 21
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Just 28
  , Just 29
  , Just 30
  , Just 31
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Just 0
  ]

type Drwng = Drawing PixelRGBA8 ()

color r g b a = uniformTexture $ PixelRGBA8 r g b a

black = color 0 0 0 0xff

drawing :: Drwng
drawing =
  withTexture black $ fill $ rectangle (V2 (-20) 0) 40 400

main :: IO ()
main = do
  let white = PixelRGBA8 255 255 255 255
      size = 800
      img = renderDrawing size size white $
            withTransformation (translate $ V2 400 20) $ drawing
  writePng "pin-diagram.png" img
