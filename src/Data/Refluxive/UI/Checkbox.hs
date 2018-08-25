{-|
  Checkbox component
-}
{-# LANGUAGE CPP #-}
module Data.Refluxive.UI.Checkbox
  (
  -- * Model
    checkState

  -- * Event
  , onChanged
  ) where

import qualified SDL as SDL
import SDL.Vect
import Control.Lens
import Control.Monad.State
import Data.Ix (inRange)
import Graphics.UI.Refluxive

#include "macro.h"

-- | Checkbox state (checked? or not)
MAKE_LENS("checkbox",Bool,checkState,_checkState)

-- | Change event listener for checkbox component
-- | The third argument is a callback, which recieves 'RenderState' and current (after-changed) checkbox state
onChanged :: Component UI tgt => ComponentView "checkbox" -> ComponentView tgt -> (RenderState -> Bool -> StateT (Model tgt) UI ()) -> UI ()
onChanged btn tgt callback = addWatchSignal tgt $ watch btn $ \rs -> \case
  Changed b -> callback rs b

instance Component UI "checkbox" where
  type ModelParam "checkbox" = ()
  data Model "checkbox" = CheckBoxModel { _checkState :: Bool }
  data Signal "checkbox" = Changed Bool

  newModel () = return $ CheckBoxModel False

  initComponent self = do
    b <- use _builtIn

    addWatchSignal self $ watch b $ \rs -> \case
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        when (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + V2 30 30) (fmap fromEnum v)) $ do
          modify $ \(CheckBoxModel b) -> CheckBoxModel (not b)

          CheckBoxModel b <- get
          lift $ emit self $ Changed b
      _ -> return ()

  getGraphical (CheckBoxModel b) = do
    return $ graphics
      [ if b
        then colored (V4 50 150 50 255) $ textDef "✓"
        else colored (V4 200 200 200 255) $ textDef "□"
      ]
