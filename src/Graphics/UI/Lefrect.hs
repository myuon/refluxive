{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Lefrect
  ( module Graphics.UI.Lefrect.Graphical
  , module Graphics.UI.Lefrect.Component
  , Signal(..)

  , UI
  , runUI
  , register
  , clear
  , emit
  , watch
  , mainloop
  , rawGraphical
  ) where

import qualified SDL as SDL
import qualified SDL.Font as SDLF
import Control.Concurrent.STM.TChan
import Control.Lens hiding (view)
import Control.Monad.STM
import Control.Monad.State
import Control.Monad.Cont
import Data.Word (Word8)
import qualified Data.Vector.Mutable as V
import qualified Data.IntSet as S
import qualified Data.Map as M
import Linear.V4
import Unsafe.Coerce
import Graphics.UI.Lefrect.Graphical
import Graphics.UI.Lefrect.Component

data Registry a
  = Registry
  { _content :: V.IOVector a
  , _keys :: S.IntSet
  , _uids :: M.Map String Int
  }

makeLenses ''Registry

newRegistry :: IO (Registry a)
newRegistry = do
  v <- V.new 20

  return $ Registry
    { _content = v
    , _keys = S.empty
    , _uids = M.empty
    }

pushRegistry :: String -> a -> Registry a -> IO (Int, Registry a)
pushRegistry name a reg = (`runContT` return) $ callCC $ \return_ -> do
  forM_ [0..V.length (reg ^. content)] $ \i -> do
    when (i `S.notMember` (reg ^. keys)) $ do
      V.write (reg ^. content) i a
      return_ (i, reg & keys %~ S.insert i & uids %~ M.insert name i)

  let length = V.length (reg ^. content)
  content' <- liftIO $ V.grow (reg ^. content) 20
  liftIO $ V.write content' length a
  return $ (length, reg & content .~ content' & keys %~ S.insert length & uids %~ M.insert name length)

getRegistryByUID :: MonadIO m => String -> Registry a -> m a
getRegistryByUID uid reg = liftIO $ V.read (reg ^. content) (reg ^. uids ^?! ix uid)

modifyMRegistryByUID :: MonadIO m => String -> Registry a -> (a -> m a) -> m ()
modifyMRegistryByUID uid reg updater = do
  current <- getRegistryByUID uid reg
  next <- updater current
  liftIO $ V.write (reg ^. content) (reg ^. uids ^?! ix uid) next

data SomeSignal = forall a. Component UI a => SomeSignal (Signal a)
newtype EventStream = EventStream { getEventStream :: TChan (String, SomeSignal) }

newEventStream :: MonadIO m => m EventStream
newEventStream = fmap EventStream $ liftIO newTChanIO

pushEvent :: (Component UI a, MonadIO m) => EventStream -> Signal a -> m ()
pushEvent stream s = liftIO $ atomically $ writeTChan (getEventStream stream) (uid s, SomeSignal s)

pullEvent :: MonadIO m => EventStream -> m (Maybe (String, SomeSignal))
pullEvent stream = liftIO $ atomically $ tryReadTChan (getEventStream stream)

instance Component UI "builtin" where
  data Signal "builtin" = BuiltInSignal SDL.Event

newtype UI a = UI { unpackUI :: StateT UIState IO a } deriving (Functor, Applicative, Monad, MonadIO)
data SomeComponent = forall a. Component UI a => SomeComponent (ComponentView a)
data SomeCallback = forall a b. (Component UI a, Component UI b) => SomeCallback String (Signal a -> StateT (Model b) UI ())

data UIState
  = UIState
  { _renderer :: SDL.Renderer
  , _registry :: Registry SomeComponent
  , _eventStream :: EventStream
  , _distributer :: M.Map String [SomeCallback]
  , _font :: Maybe SDLF.Font
  }

makeLenses ''UIState

runUI :: UI () -> IO ()
runUI m = do
  SDL.initializeAll
  SDLF.initialize

  evalStateT (unpackUI m) =<< UIState
    <$> ((\w -> SDL.createRenderer w (-1) SDL.defaultRenderer) =<< SDL.createWindow "window" SDL.defaultWindow)
    <*> newRegistry
    <*> newEventStream
    <*> pure M.empty
    <*> fmap Just (SDLF.load "/usr/share/fonts/truetype/ubuntu/Ubuntu-M.ttf" 20)

register :: Component UI a => ComponentView a -> UI ()
register cp = do
  UI $ do
    reg <- use registry
    (_, reg') <- liftIO $ pushRegistry (uid cp) (SomeComponent cp) reg
    registry .= reg'

  mapM_ addWatchSignal $ watcher cp

clear :: V4 Word8 -> UI ()
clear c = UI $ do
  r <- use renderer
  SDL.rendererDrawColor r SDL.$= c
  SDL.clear r

emit :: Component UI a => Signal a -> UI ()
emit s = UI $ use eventStream >>= \es -> liftIO (pushEvent es s)

mainloop :: UI ()
mainloop = do
  events <- SDL.pollEvents
  keyQuit <- return $ flip any events $ \ev -> case SDL.eventPayload ev of
    SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym SDL.ScancodeQ _ _)) -> True
    _ -> False

  -- pour events into stream
  es <- UI $ use eventStream
  UI $ liftIO $ mapM_ (pushEvent es . BuiltInSignal) events

  -- clear
  clear (V4 30 100 200 255)

  -- render all components
  reg <- UI $ use registry
  forM_ (S.elems $ reg ^. keys) $ \i -> do
    SomeComponent cp <- liftIO $ V.read (reg ^. content) i

    r <- UI $ use renderer
    f <- UI $ use font
    view cp >>= render f r

  -- commit view changes
  SDL.present =<< UI (use renderer)

  -- event handling
  events <- UI $ pullEvent =<< use eventStream
  case events of
    Just (src, SomeSignal signal) -> do
      callbacks <- UI $ fmap (\d -> if M.member src d then d M.! src else []) $ use distributer
      r <- UI $ use registry

      forM_ callbacks $ \(SomeCallback tgt cb) -> do
        modifyMRegistryByUID tgt r $ \(SomeComponent cp) -> do
          model' <- flip execStateT (model cp) $ unsafeCoerce cb signal
          return $ SomeComponent $ cp { model = model' }
    Nothing -> return ()

  -- quit?
  unless keyQuit mainloop

  SDLF.quit
  SDL.quit

watch :: (Component UI src, Component UI tgt) => String -> (Signal src -> StateT (Model tgt) UI ()) -> Watcher UI tgt
watch = Watcher

addWatchSignal :: Component UI tgt => Watcher UI tgt -> UI ()
addWatchSignal (w@(Watcher name callback)) = UI $ do
  d <- use distributer
  when (name `M.notMember` d) $ distributer %= M.insert name []
  distributer . ix name %= (:) (SomeCallback (getWatcherTgtUID w) callback)

instance Component UI "raw" where
  data Model "raw" = RawModel Graphical
  data Signal "raw"

  setup = do
    cp <- liftIO $ new $ RawModel empty
    return cp

  getGraphical (RawModel g) = return g

rawGraphical :: ComponentView "raw" -> Graphical -> ComponentView "raw"
rawGraphical cp g = cp { model = RawModel g }


