{-|
  Button widget
-}
module Data.Refluxive.UI.Button
  (
    ButtonState(..)
  , Signal(..)
  , Model(..)
  ) where

import qualified SDL as SDL
import qualified SDL.Primitive as SDLP
import SDL.Vect
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.State
import Data.Ix (inRange)
import qualified Data.Text as T
import Data.Extensible
import Graphics.UI.Refluxive

-- | internal state
data ButtonState = None | Hover | Clicking
  deriving Eq

instance Component UI "button" where
  type ModelParam "button" = Record
    [ "label" >: T.Text
    , "size" >: SDLP.Pos
    ]

  data Model "button" = ButtonModel
    { label :: T.Text
    , size :: SDLP.Pos
    , buttonState :: ButtonState
    }

  data Signal "button" = Click

  newModel param = return $ ButtonModel
    { label = param ^. #label
    , size = param ^. #size
    , buttonState = None
    }

  initComponent self = do
    b <- use _builtIn

    addWatchSignal self $ watch b $ \rs -> \case
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        model <- get
        when (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + size model) (fmap fromEnum v)) $ do
          modify $ \model -> model { buttonState = Clicking }
          lift $ emit self Click
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        model <- get
        if (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + size model) (fmap fromEnum v))
          then modify $ \model -> model { buttonState = Hover }
          else modify $ \model -> model { buttonState = None }
      BuiltInSignal (SDL.Event _ (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ (SDL.P v) _))) -> do
        model <- get
        if (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + size model) (fmap fromEnum v))
          then modify $ \model -> model { buttonState = Hover }
          else modify $ \model -> model { buttonState = None }
      _ -> return ()

  getGraphical model = do
    return $ graphics $
      [ colored (if buttonState model == Hover then V4 220 220 220 255 else V4 200 200 200 255) $ rectangle (V2 0 0) (size model)
      , colored (V4 0 0 0 255) $ rectangleWith (#fill @= False <: nil) (V2 0 0) (size model)
      , translate (V2 10 5) $ text $ label model
      ]

