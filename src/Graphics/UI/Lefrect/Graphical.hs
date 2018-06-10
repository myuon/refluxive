{-# LANGUAGE TypeOperators, OverloadedLabels, FlexibleContexts, PolyKinds #-}
module Graphics.UI.Lefrect.Graphical
  ( Graphical

  , empty
  , gridLayout
  , rectangle
  , rectangleWith
  , colored
  , translate
  , graphics

  , render
  ) where

import qualified SDL as SDL
import qualified SDL.Primitive as SDL
import Linear.V2
import Control.Lens ((^.), (.~), (&))
import Control.Monad.Trans
import Data.Extensible
import Foreign.C.Types

type ShapeStyle =
  [ "fill" >: Bool
  , "rounded" >: Maybe Int
  ]

defShapeStyle :: Record ShapeStyle
defShapeStyle
  = #fill @= True
  <: #rounded @= Nothing
  <: nil

data Graphical
  = Empty
  | GridLayout SDL.Pos Graphical
  | Rectangle (Record ShapeStyle) SDL.Pos SDL.Pos
  | Colored SDL.Color Graphical
  | Translate SDL.Pos Graphical
  | Graphics [Graphical]

data RenderState
  = RenderState
  { color :: SDL.Color
  , coordinate :: SDL.Pos
  , scaler :: SDL.Pos
  }

defRenderState :: RenderState
defRenderState
  = RenderState
  { color = SDL.V4 255 255 255 255
  , coordinate = SDL.V2 0 0
  , scaler = SDL.V2 1 1
  }

render :: MonadIO m => SDL.Renderer -> Graphical -> m ()
render renderer = go defRenderState where
  go :: MonadIO m => RenderState -> Graphical -> m ()
  go st Empty = return ()
  go st (GridLayout s g) = go (st { scaler = s }) g
  go st (Rectangle style pos size) =
    let topLeft = pos * scaler st + coordinate st in
    let bottomRight = (pos + size) * scaler st + coordinate st in
    case (style ^. #fill, fmap toEnum $ style ^. #rounded) of
      (True, Just r) -> SDL.fillRoundRectangle renderer topLeft bottomRight r (color st)
      (True, Nothing) -> SDL.fillRectangle renderer topLeft bottomRight (color st)
      (False, Just r) -> SDL.roundRectangle renderer topLeft bottomRight r (color st)
      (False, Nothing) -> SDL.rectangle renderer topLeft bottomRight (color st)
  go st (Colored color g) = go (st { color = color }) g
  go st (Translate p g) = go (st { coordinate = coordinate st + p * scaler st }) g
  go st (Graphics gs) = mapM_ (go st) gs

empty :: Graphical
empty = Empty

gridLayout :: SDL.Pos -> Graphical -> Graphical
gridLayout = GridLayout

rectangleWith :: IncludeAssoc ShapeStyle xs => Record xs -> SDL.Pos -> SDL.Pos -> Graphical
rectangleWith cfg = Rectangle (hmergeAssoc cfg defShapeStyle)
  where
    hmergeAssoc :: (IncludeAssoc ys xs, Wrapper h) => h :* xs -> h :* ys -> h :* ys
    hmergeAssoc hx hy = hfoldrWithIndex (\xin x hy -> hy & itemAt (hlookup xin inclusionAssoc) .~ x^._Wrapper) hy hx

rectangle :: SDL.Pos -> SDL.Pos -> Graphical
rectangle = rectangleWith nil

colored :: SDL.Color -> Graphical -> Graphical
colored = Colored

translate :: SDL.Pos -> Graphical -> Graphical
translate = Translate

graphics :: [Graphical] -> Graphical
graphics = Graphics
