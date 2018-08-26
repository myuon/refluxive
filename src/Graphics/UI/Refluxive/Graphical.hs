{-# LANGUAGE TypeOperators, OverloadedLabels, FlexibleContexts, PolyKinds #-}
{-|
  Provides graphical functions
-}
module Graphics.UI.Refluxive.Graphical
  (
  -- * Graphical types
    Graphical
  , RenderState(..)
  , render

  -- * Drawing functions
  , empty
  , lineTo
  , relLineTo
  , text
  , textWith
  , textDef
  , textDefWith
  , gridLayout
  , rectangle
  , rectangleWith
  , SDLF.Style(..)
  , colored
  , translate
  , graphics
  , clip
  , viewInfo

  -- * Constructors
  , pattern TextStyle, fill, rounded
  , pattern ShapeStyle, styles
  , Name(..)
  ) where

import qualified SDL as SDL
import qualified SDL.Primitive as SDL
import qualified SDL.Font as SDLF
import Linear.V2
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe
import Data.Default
import Foreign.C.Types
import System.Mem

data Name = Name String String
  deriving (Show, Eq, Ord)

newtype ShapeStyleType = ShapeStyleType (Bool, Maybe Int)

-- | Shape style for 'rectangle' object
pattern ShapeStyle :: Bool -> Maybe Int -> ShapeStyleType
pattern ShapeStyle{ fill, rounded } = ShapeStyleType (fill, rounded)

instance Default ShapeStyleType where
  def = ShapeStyle { fill = True, rounded = Nothing }

newtype TextStyleType = TextStyleType [SDLF.Style]

-- | Text style for 'text' object
pattern TextStyle :: [SDLF.Style] -> TextStyleType
pattern TextStyle { styles } = TextStyleType styles

instance Default TextStyleType where
  def = TextStyle { styles = [] }

-- | 'Graphical' object can be displayed on screen with 'render'
data Graphical
  = Empty
  | GridLayout SDL.Pos Graphical
  | Rectangle ShapeStyleType SDL.Pos SDL.Pos
  | Colored SDL.Color Graphical
  | Translate SDL.Pos Graphical
  | Graphics [Graphical]
  | Text TextStyleType String Int T.Text
  | Clip SDL.Pos Graphical
  | ViewInfo Name Graphical
  | LineTo SDL.Pos SDL.Pos
  | RelLineTo SDL.Pos SDL.Pos

-- | A rendering context
data RenderState
  = RenderState
  { color :: SDL.Color
  , coordinate :: SDL.Pos
  , scaler :: SDL.Pos
  }

instance Default RenderState where
  def
    = RenderState
    { color = SDL.V4 0 0 0 255
    , coordinate = SDL.V2 0 0
    , scaler = SDL.V2 1 1
    }

-- | A function to draw 'Graphical' objects
render :: MonadIO m
       => SDLF.Color -- ^ clearColor (__not a rendering color__)
       -> M.Map (String,Int) SDLF.Font -- ^ font registry
       -> SDL.Renderer -- ^ current renderer
       -> Graphical -- ^ object to render
       -> (RenderState -> Name -> m ()) -- ^ tagging function which is used in Components
       -> m ()
render clearColor fonts renderer g cont = go def g >> liftIO performGC where
  go st Empty = return ()
  go st (GridLayout s g) = go (st { scaler = s }) g
  go st (Rectangle style pos size) = do
    let topLeft = pos * scaler st + coordinate st
    let bottomRight = (pos + size) * scaler st + coordinate st
    case (fill style, fmap toEnum $ rounded style) of
      (True, Just r) -> SDL.fillRoundRectangle renderer topLeft bottomRight r (color st)
      (True, Nothing) -> SDL.fillRectangle renderer topLeft bottomRight (color st)
      (False, Just r) -> SDL.roundRectangle renderer topLeft bottomRight r (color st)
      (False, Nothing) -> SDL.rectangle renderer topLeft bottomRight (color st)
  go st (Colored color g) = go (st { color = color }) g
  go st (Translate p g) = go (st { coordinate = coordinate st + p * scaler st }) g
  go st (Graphics gs) = mapM_ (go st) gs
  go st (Text ts name size txt) | T.null txt = return ()
  go st (Text ts name size txt) = do
    SDLF.setStyle (fonts M.! (name,size)) (styles ts)
    surface <- SDLF.blended (fonts M.! (name,size)) (color st) txt
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend
    tinfo <- SDL.queryTexture texture
    let size = V2 (SDL.textureWidth tinfo) (SDL.textureHeight tinfo)
    SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P (coordinate st)) size)
    SDL.freeSurface surface
  go st (Clip size g) = do
    texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget size
    SDL.rendererRenderTarget renderer SDL.$= Just texture
    SDL.rendererDrawColor renderer SDL.$= clearColor
    SDL.clear renderer
    go (st { coordinate = SDL.V2 0 0 }) g
    SDL.rendererRenderTarget renderer SDL.$= Nothing
    SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P (coordinate st)) size)
  go st (ViewInfo v g) = cont st v >> go st g
  go st (LineTo p q) = SDL.smoothLine renderer (coordinate st + p) (coordinate st + q) (color st)
  go st (RelLineTo p q) = SDL.smoothLine renderer (coordinate st + p) (coordinate st + p + q) (color st)

-- | Empty object
empty :: Graphical
empty = Empty

-- | Text object (font family and size is currently hard-coded)
text :: String -> Int -> T.Text -> Graphical
text = textWith def

-- | Text object with text style
textWith :: TextStyleType -> String -> Int -> T.Text -> Graphical
textWith = Text

-- | Text object with default Font
textDef :: T.Text -> Graphical
textDef = text "def" 12

-- | Text object with default Font, text style
textDefWith :: TextStyleType -> T.Text -> Graphical
textDefWith s = textWith s "def" 12

-- | Scaling function, useful to place objects in grid
gridLayout :: SDL.Pos -> Graphical -> Graphical
gridLayout = GridLayout

-- | Line object
lineTo :: SDL.Pos -> SDL.Pos -> Graphical
lineTo = LineTo

-- | Line object, as target position is calculated relatively
relLineTo :: SDL.Pos -> SDL.Pos -> Graphical
relLineTo = RelLineTo

-- | Rectangle object with shape style
rectangleWith :: ShapeStyleType -- ^ shape style
              -> SDL.Pos -- ^ top-left coordinate
              -> SDL.Pos -- ^ size
              -> Graphical
rectangleWith = Rectangle

-- | Rectangle object
--
-- > rectangle == rectangleWith nil
rectangle :: SDL.Pos -> SDL.Pos -> Graphical
rectangle = rectangleWith def

-- | Coloring function
colored :: SDL.Color -> Graphical -> Graphical
colored = Colored

-- | Coordinate translating function
translate :: SDL.Pos -> Graphical -> Graphical
translate = Translate

-- | Many graphical objects
graphics :: [Graphical] -> Graphical
graphics = Graphics

-- | Clipping function
clip :: SDL.Pos -> Graphical -> Graphical
clip = Clip

-- | Tagging id with given graphical (like CSS id)
viewInfo :: Name -> Graphical -> Graphical
viewInfo = ViewInfo

