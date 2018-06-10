{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Lefrect
  ( module Graphics.UI.Lefrect.Graphical
  , module Graphics.UI.Lefrect.Component

  , UI
  , runUI
  , register
  , clear
  , mainloop
  ) where

import qualified SDL as SDL
import Control.Concurrent.MVar
import Control.Lens hiding (view)
import Control.Monad.State
import Control.Monad.Cont
import Data.Word (Word8)
import qualified Data.Vector.Mutable as V
import qualified Data.IntSet as S
import Linear.V4
import Graphics.UI.Lefrect.Graphical
import Graphics.UI.Lefrect.Component

data Registry a
  = Registry
  { _content :: V.IOVector a
  , _keys :: S.IntSet
  }

makeLenses ''Registry

newRegistry :: IO (Registry a)
newRegistry = do
  v <- V.new 20

  return $ Registry
    { _content = v
    , _keys = S.empty
    }

pushRegistry :: a -> Registry a -> IO (Int, Registry a)
pushRegistry a reg = (`runContT` return) $ callCC $ \return_ -> do
  forM_ [0..V.length (reg ^. content)] $ \i -> do
    when (i `S.notMember` (reg ^. keys)) $ do
      V.write (reg ^. content) i a
      return_ (i, reg & keys %~ S.insert i)

  let length = V.length (reg ^. content)
  content' <- liftIO $ V.grow (reg ^. content) 20
  liftIO $ V.write content' length a
  return $ (length, reg & content .~ content' & keys %~ S.insert length)

data SomeSignal = forall a. Component a => SomeSignal (Signal a)
newtype EventStream = EventStream { getEventStream :: MVar [(String, SomeSignal)] }

newEventStream :: IO EventStream
newEventStream = fmap EventStream $ newMVar []

pushEvent :: Component a => EventStream -> Signal a -> IO ()
pushEvent stream s = modifyMVar_ (getEventStream stream) (\es -> return $ (uid s, SomeSignal s) : es)

pullEvents :: EventStream -> IO [(String, SomeSignal)]
pullEvents stream = do
  es <- readMVar (getEventStream stream)
  putMVar (getEventStream stream) []
  return es

newtype UI a = UI { unpackUI :: StateT UIState IO a } deriving (Functor, Applicative, Monad, MonadIO)

data SomeComponent = forall a. Component a => SomeComponent (ComponentView a)

data UIState
  = UIState
  { _renderer :: SDL.Renderer
  , _registry :: Registry SomeComponent
  , _eventStream :: EventStream
  }

makeLenses ''UIState

runUI :: UI () -> IO ()
runUI m = do
  SDL.initializeAll

  evalStateT (unpackUI m) =<< UIState
    <$> ((\w -> SDL.createRenderer w (-1) SDL.defaultRenderer) =<< SDL.createWindow "window" SDL.defaultWindow)
    <*> newRegistry
    <*> newEventStream

register :: Component a => View a -> UI ()
register v = do
  let cp = getComponentView v
  UI $ do
    reg <- use registry
    (_, reg') <- liftIO $ pushRegistry (SomeComponent cp) reg
    registry .= reg'

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

  -- clear
  clear (V4 30 100 200 255)

  -- render all components
  r <- UI $ use renderer
  reg <- UI $ use registry
  forM_ (S.elems $ reg ^. keys) $ \i -> liftIO $ do
    SomeComponent cp <- V.read (reg ^. content) i
    view cp >>= render r

  -- commit view changes
  SDL.present =<< UI (use renderer)

  -- event handling
  reg <- UI $ use registry
  forM_ (S.elems $ reg ^. keys) $ \i -> liftIO $ do
    SomeComponent cp <- V.read (reg ^. content) i
    V.write (reg ^. content) i . SomeComponent =<< exec cp

  -- quit?
  unless keyQuit mainloop

