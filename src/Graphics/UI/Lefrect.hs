{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Lefrect
  ( module Graphics.UI.Lefrect.Graphical
  , module Graphics.UI.Lefrect.Component

  , UI
  , runUI
  , ui
  , clear
  , mainloop
  ) where

import qualified SDL as SDL
import Control.Monad.State
import Control.Lens
import Data.Word
import Linear.V4
import Graphics.UI.Lefrect.Graphical
import Graphics.UI.Lefrect.Component

newtype UI a = UI { unpackUI :: StateT UIState IO a } deriving (Functor, Applicative, Monad, MonadIO)

data UIState
  = UIState
  { _window :: SDL.Window
  , _renderer :: SDL.Renderer
  , _uiRegistry :: [(Layout, Graphical)]
  }

makeLenses ''UIState

runUI :: UI () -> IO ()
runUI m = do
  SDL.initializeAll
  window <- SDL.createWindow "window" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  evalStateT (unpackUI m) $ UIState
    { _window = window
    , _renderer = renderer
    , _uiRegistry = []
    }

ui :: Layout -> Graphical -> UI ()
ui layout graphical = UI $ do
  uiRegistry %= ((layout, graphical) :)

clear :: V4 Word8 -> UI ()
clear c = UI $ do
  r <- use renderer
  SDL.rendererDrawColor r SDL.$= c
  SDL.clear r

mainloop :: UI ()
mainloop = do
  events <- SDL.pollEvents
  keyQuit <- return $ flip any events $ \ev -> case SDL.eventPayload ev of
    SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym SDL.ScancodeQ _ _)) -> True
    _ -> False

  clear (V4 30 100 200 255)

  r <- UI $ use renderer
  mapM_ (uncurry (render r)) =<< UI (use uiRegistry)

  SDL.present =<< UI (use renderer)

  unless keyQuit mainloop

