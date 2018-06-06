module Graphical
  ( Layout(..)
  , Graphical

  , rectangle
  , colored
  , translate
  , graphics

  , render
  ) where

import qualified SDL as SDL
import qualified SDL.Primitive as SDL
import Linear.V2
import Control.Monad.Trans
import Foreign.C.Types

data Layout
  = GridLayout CInt CInt
  | Pixel

calcCoordinate :: Layout -> SDL.Pos -> SDL.Pos
calcCoordinate Pixel v = v
calcCoordinate (GridLayout sx sy) v = V2 sx sy * v

data Graphical
  = Rectangle SDL.Pos SDL.Pos
  | Colored SDL.Color Graphical
  | Translate SDL.Pos Graphical
  | Graphics [Graphical]

data RenderState
  = RenderState
  { color :: SDL.Color
  , coordinate :: SDL.Pos
  }

defRenderState :: RenderState
defRenderState
  = RenderState
  { color = SDL.V4 255 255 255 255
  , coordinate = SDL.V2 0 0
  }

render :: MonadIO m => SDL.Renderer -> Layout -> Graphical -> m ()
render renderer layout = go defRenderState where
  go st (Rectangle pos size) = SDL.fillRectangle
    renderer
    (calcCoordinate layout (pos) + coordinate st)
    (calcCoordinate layout (pos + size) + coordinate st)
    (color st)
  go st (Colored color g) = go (st { color = color }) g
  go st (Translate p g) = go (st { coordinate = coordinate st + p }) g
  go st (Graphics gs) = mapM_ (go st) gs

rectangle :: SDL.Pos -> SDL.Pos -> Graphical
rectangle = Rectangle

colored :: SDL.Color -> Graphical -> Graphical
colored = Colored

translate :: SDL.Pos -> Graphical -> Graphical
translate = Translate

graphics :: [Graphical] -> Graphical
graphics = Graphics

