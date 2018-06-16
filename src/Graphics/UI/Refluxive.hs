{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Refluxive
  ( module Graphics.UI.Refluxive.Graphical

  , Component(..)
  , ComponentView
  , Watcher

  , Signal(..)
  , Root(..)

  , UI
  , runUI
  , register
  , emit
  , watch
  , mainloop
  , rawGraphical
  , setClearColor
  , quit

  , new
  , view
  , operateModel
  ) where

import qualified SDL as SDL
import qualified SDL.Font as SDLF
import Control.Concurrent.Chan.Unagi.NoBlocking
import Control.Lens hiding (view)
import Control.Monad.State
import Control.Monad.Cont
import Data.Word (Word8)
import qualified Data.Vector.Mutable as V
import qualified Data.IntSet as S
import qualified Data.Map as M
import Linear.V4
import Unsafe.Coerce
import Graphics.UI.Refluxive.Graphical
import Graphics.UI.Refluxive.Component

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
newtype EventStream = EventStream { getEventStream :: (InChan (String, SomeSignal), OutChan (String, SomeSignal)) }

newEventStream :: MonadIO m => m EventStream
newEventStream = fmap EventStream $ liftIO newChan

pushEvent :: (Component UI a, MonadIO m) => EventStream -> Signal a -> m ()
pushEvent stream s = liftIO $ writeChan (fst $ getEventStream stream) $ makeEvent s

pushEvents :: MonadIO m => EventStream -> [(String, SomeSignal)] -> m ()
pushEvents stream s = liftIO $ writeList2Chan (fst $ getEventStream stream) s

makeEvent :: Component UI a => Signal a -> (String, SomeSignal)
makeEvent s = (uid s, SomeSignal s)

pullEvent :: MonadIO m => EventStream -> m (Maybe (String, SomeSignal))
pullEvent stream = liftIO $ tryRead =<< tryReadChan (snd $ getEventStream stream)

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
  , _clearColor :: SDLF.Color
  , _isQuit :: Bool
  }

makeLenses ''UIState

instance Component UI "builtin" where
  data Signal "builtin" = BuiltInSignal SDL.Event

instance MonadState UIState UI where
  state = UI . state

runUI :: UI () -> IO ()
runUI m = do
  SDL.initializeAll
  SDLF.initialize

  evalStateT (unpackUI $ initialize >> m) =<< UIState
    <$> ((\w -> SDL.createRenderer w (-1) SDL.defaultRenderer) =<< SDL.createWindow "window" (SDL.defaultWindow { SDL.windowResizable = True }))
    <*> newRegistry
    <*> newEventStream
    <*> pure M.empty
    <*> fmap Just (SDLF.load "/usr/share/fonts/truetype/ubuntu/Ubuntu-M.ttf" 20)
    <*> pure (V4 255 255 255 255)
    <*> pure False

setClearColor :: SDLF.Color -> UI ()
setClearColor c = do
  r <- use renderer
  clearColor .= c
  SDL.rendererDrawColor r SDL.$= c

register :: Component UI a => ComponentView a -> UI ()
register cp = do
  reg <- use registry
  (_, reg') <- liftIO $ pushRegistry (uid cp) (SomeComponent cp) reg
  registry .= reg'

  listen cp

listen :: Component UI a => ComponentView a -> UI ()
listen cp = mapM_ addWatchSignal $ watcher cp

emit :: Component UI a => Signal a -> UI ()
emit s = use eventStream >>= \es -> liftIO (pushEvent es s)

clear :: UI ()
clear = do
  r <- use renderer
  c <- use clearColor
  SDL.rendererDrawColor r SDL.$= c
  SDL.clear r

quit :: UI ()
quit = isQuit .= True

data Root = All | RootUIDs [String]

initialize :: UI ()
initialize = do
  use clearColor >>= setClearColor

  es <- use eventStream
  SDL.addEventWatch $ pushEvent es . BuiltInSignal

  return ()

mainloop :: Root -> UI ()
mainloop root = do
  ev <- SDL.pollEvent
  case ev of
    Just ev -> do
      es <- use eventStream
      pushEvent es $ BuiltInSignal ev
    _ -> return ()

  -- clear
  clear

  -- render all components
  keys <- do
    reg <- use registry
    return $ case root of
      All -> S.elems $ reg ^. keys
      RootUIDs xs -> fmap ((reg ^. uids) M.!) xs

  forM_ keys $ \i -> do
    reg <- use registry
    SomeComponent cp <- liftIO $ V.read (reg ^. content) i

    r <- use renderer
    f <- use font
    c <- use clearColor
    render c f r =<< view cp

  -- commit view changes
  use renderer >>= SDL.present

  -- event handling
  events <- pullEvent =<< use eventStream
  case events of
    Just (src, SomeSignal signal) -> do
      liftIO $ print "pullEvent"
      callbacks <- fmap (\d -> if M.member src d then d M.! src else []) $ use distributer

      r <- use registry
      forM_ callbacks $ \(SomeCallback tgt cb) -> do
        modifyMRegistryByUID tgt r $ \(SomeComponent cp) -> do
          model' <- flip execStateT (model cp) $ unsafeCoerce cb signal
          return $ SomeComponent $ cp { model = model' }
    Nothing -> return ()

  -- wait
  SDL.delay 33

  q <- use isQuit
  unless q $ mainloop root

  SDLF.quit
  SDL.quit

watch :: (Component UI src, Component UI tgt) => String -> (Signal src -> StateT (Model tgt) UI ()) -> Watcher UI tgt
watch = Watcher

addWatchSignal :: Component UI tgt => Watcher UI tgt -> UI ()
addWatchSignal (w@(Watcher name callback)) = do
  d <- use distributer
  when (name `M.notMember` d) $ distributer %= M.insert name []
  distributer . ix name %= (:) (SomeCallback (getWatcherTgtUID w) callback)

instance Component UI "raw" where
  data Model "raw" = RawModel Graphical
  data Signal "raw"

  setup = do
    cp <- new $ RawModel empty
    return cp

  getGraphical (RawModel g) = return g

rawGraphical :: ComponentView "raw" -> Graphical -> ComponentView "raw"
rawGraphical cp g = cp { model = RawModel g }

new :: Model a -> UI (ComponentView a)
new model = do
  return $ ComponentView
    { model = model
    }

view :: (Component UI a) => ComponentView a -> UI Graphical
view cp = do
  r <- use registry
  SomeComponent cp <- getRegistryByUID (uid cp) r
  getGraphical (model cp)

operateModel :: Component UI a => ComponentView a -> StateT (Model a) UI () -> UI ()
operateModel cp f = do
  r <- use registry
  modifyMRegistryByUID (uid cp) r $ \(SomeComponent cp) -> do
    model' <- flip execStateT (model cp) $ unsafeCoerce f
    return $ SomeComponent $ cp { model = model' }

