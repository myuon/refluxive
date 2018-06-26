{-|
  Button component
-}
{-# LANGUAGE CPP #-}
module Data.Refluxive.UI.Button
  (
  -- * Model
  label
  , size
  , buttonState
  , ButtonState(..)

  -- * Event
  , onClick
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

-- | Internal state
data ButtonState = None | Hover | Clicking
  deriving Eq

-- | Click event listener for button component
onClick :: Component UI tgt => ComponentView "button" -> ComponentView tgt -> (RenderState -> StateT (Model tgt) UI ()) -> UI ()
onClick btn tgt callback = addWatchSignal tgt $ watch btn $ \rs -> \case
  Click -> callback rs

#include "macro.h"

-- | Button Label
MAKE_LENS("button",T.Text,label,_label)

-- | Button Size
MAKE_LENS("button",SDLP.Pos,size,_size)

-- | 'ButtonState'
MAKE_LENS("button",ButtonState,buttonState,_buttonState)

instance Component UI "button" where
  type ModelParam "button" = Record
    [ "label" >: T.Text
    , "size" >: SDLP.Pos
    ]

  data Model "button" = ButtonModel
    { _label :: T.Text
    , _size :: SDLP.Pos
    , _buttonState :: ButtonState
    }

  data Signal "button" = Click

  newModel param = return $ ButtonModel
    { _label = param ^. #label
    , _size = param ^. #size
    , _buttonState = None
    }

  initComponent self = do
    b <- use _builtIn

    addWatchSignal self $ watch b $ \rs -> \case
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        model <- get
        when (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + model^.size) (fmap fromEnum v)) $ do
          buttonState .= Clicking
          lift $ emit self Click
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        model <- get
        if (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + model^.size) (fmap fromEnum v))
          then buttonState .= Hover
          else buttonState .= None
      BuiltInSignal (SDL.Event _ (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ (SDL.P v) _))) -> do
        model <- get
        if (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + model^.size) (fmap fromEnum v))
          then buttonState .= Hover
          else buttonState .= None
      _ -> return ()

  getGraphical model = do
    return $ graphics $
      [ colored (if model^.buttonState == Hover then V4 220 220 220 255 else V4 200 200 200 255) $ rectangle (V2 0 0) (model^.size)
      , colored (V4 0 0 0 255) $ rectangleWith (#fill @= False <: nil) (V2 0 0) (model^.size)
      , translate (V2 10 5) $ text $ model^.label
      ]



