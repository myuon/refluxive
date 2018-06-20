{-|
  Refluxive framework
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Refluxive
  (
  -- * UI monad
  UI
  , runUI
  , mainloop

  , register
  , emit
  , watch
  , asRoot
  , setClearColor
  , addWatchSignal
  , quit

  -- * Component "raw"
  , rawGraphical


  -- * Re-exports
  , module Graphics.UI.Refluxive.Graphical

  -- * Component types
  , Component(..)
  , ComponentView
  , Watcher
  , getModel

  -- * Signal
  , Signal(..)
  , _builtIn

  -- * Operations on component
  , new
  , view
  , operateModel
  ) where

import qualified SDL as SDL
import qualified SDL.Font as SDLF
import Control.Concurrent.MVar
import Control.Lens hiding (view)
import Control.Monad.State
import Control.Monad.Cont
import Data.IORef
import Data.Unique
import qualified Data.Vector.Mutable as V
import qualified Data.IntSet as S
import qualified Data.Map as M
import Data.Word (Word8)
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
getRegistryByUID uid reg = do
  when (uid `M.notMember` (reg ^. uids)) $ error $ "Unexpected name: " ++ uid ++ ", you might forget registering the ComponentView?"
  liftIO $ V.read (reg ^. content) (reg ^. uids ^?! ix uid)

modifyMRegistryByUID :: MonadIO m => String -> Registry a -> (a -> m a) -> m ()
modifyMRegistryByUID uid reg updater = do
  current <- getRegistryByUID uid reg
  next <- updater current
  liftIO $ V.write (reg ^. content) (reg ^. uids ^?! ix uid) next

data SomeSignal = forall a. Component UI a => SomeSignal (Signal a)
newtype EventStream = EventStream { getEventStream :: MVar [(String, SomeSignal)] }

newEventStream :: MonadIO m => m EventStream
newEventStream = fmap EventStream $ liftIO $ newMVar []

pushEvent :: (Component UI a, MonadIO m) => EventStream -> ComponentView a -> Signal a -> m ()
pushEvent stream cp s = liftIO $ modifyMVar_ (getEventStream stream) $ return . (makeEvent cp s :)

makeEvent :: Component UI a => ComponentView a -> Signal a -> (String, SomeSignal)
makeEvent cp s = (name cp, SomeSignal s)

pullEvents :: MonadIO m => EventStream -> m [(String, SomeSignal)]
pullEvents stream = liftIO $ fmap reverse $ swapMVar (getEventStream stream) []

-- | UI monad
newtype UI a = UI { unpackUI :: StateT UIState IO a } deriving (Functor, Applicative, Monad, MonadIO)

data SomeComponent = forall a. Component UI a => SomeComponent (ComponentView a)
data SomeCallback = forall a b. (Component UI a, Component UI b) => SomeCallback String (RenderState -> Signal a -> StateT (Model b) UI ())

data UIState
  = UIState
  { _renderer :: SDL.Renderer
  , _registry :: Registry SomeComponent
  , _eventStream :: EventStream
  , _distributer :: M.Map String [SomeCallback]
  , _font :: Maybe SDLF.Font
  , _clearColor :: SDLF.Color
  , _isQuit :: Bool
  , _builtIn' :: Maybe (ComponentView "builtin")
  }

makeLenses ''UIState

instance Component UI "builtin" where
  type ModelParam "builtin" = ()
  data Model "builtin" = BuiltInModel
  data Signal "builtin" = BuiltInSignal SDL.Event | TickSignal

  newModel () = return BuiltInModel
  getGraphical = error "unimplemented"

instance MonadState UIState UI where
  state = UI . state

-- | Running UI monad
runUI :: UI () -> IO ()
runUI m = do
  SDL.initializeAll
  SDLF.initialize

  evalStateT (unpackUI $ initialize >> m) =<< UIState
    <$> ((\w -> SDL.createRenderer w (-1) SDL.defaultRenderer) =<< SDL.createWindow "window" (SDL.defaultWindow { SDL.windowResizable = True }))
    <*> newRegistry
    <*> newEventStream
    <*> pure M.empty
    <*> fmap Just (SDLF.load "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc" 20)
    <*> pure (V4 255 255 255 255)
    <*> pure False
    <*> pure Nothing

-- | Set background color
setClearColor :: SDLF.Color -> UI ()
setClearColor c = do
  r <- use renderer
  clearColor .= c
  SDL.rendererDrawColor r SDL.$= c

-- | Register a ComponentView to refluxive framework
register :: Component UI a => ComponentView a -> UI ()
register cp = do
  reg <- use registry
  (_, reg') <- liftIO $ pushRegistry (name cp) (SomeComponent cp) reg
  registry .= reg'

-- | Emit a signal from a component
emit :: Component UI a => ComponentView a -> Signal a -> UI ()
emit cp s = use eventStream >>= \es -> liftIO (pushEvent es cp s)

clear :: UI ()
clear = do
  r <- use renderer
  c <- use clearColor
  SDL.rendererDrawColor r SDL.$= c
  SDL.clear r

-- | Quit the mainloop
quit :: UI ()
quit = isQuit .= True

initialize :: UI ()
initialize = do
  use clearColor >>= setClearColor
  b <- new @"builtin" ()
  builtIn' .= Just b
  register b

-- | A function which is used for passing to mainloop
asRoot :: Component UI a => ComponentView a -> SomeComponent
asRoot = SomeComponent

-- | A lens to focus on builtIn component
_builtIn :: Lens' UIState (ComponentView "builtin")
_builtIn = builtIn' . lens (\(Just a) -> a) (\_ a -> Just a)

-- | Start a mainloop, render given components as root
mainloop :: [SomeComponent] -> UI ()
mainloop root = do
  -- poll sdl events
  SDL.pollEvents >>= \evs -> forM_ evs $ \ev -> do
    es <- use eventStream
    Just b <- use builtIn'

    pushEvent es b $ BuiltInSignal ev

  -- event handling
  b <- use _builtIn
  use eventStream >>= pullEvents >>= \evs -> forM_ (makeEvent b TickSignal : evs) $ \(src, SomeSignal signal) -> do
    callbacks <- fmap (\d -> if M.member src d then d M.! src else []) $ use distributer

    r <- use registry
    forM_ callbacks $ \(SomeCallback tgt cb) -> do
      modifyMRegistryByUID tgt r $ \(SomeComponent cp) -> do
        rs <- liftIO $ readIORef $ renderStateRef cp
        model' <- flip execStateT (model cp) $ unsafeCoerce cb rs signal
        return $ SomeComponent $ cp { model = model' }

  -- clear
  clear

  -- render root component
  reg <- use registry
  forM_ (fmap (\(SomeComponent cp) -> (reg ^. uids) M.! name cp) root) $ \i -> do
    reg <- use registry
    SomeComponent cp <- liftIO $ V.read (reg ^. content) i

    r <- use renderer
    f <- use font
    c <- use clearColor
    g <- view cp
    render c f r g $ \st name -> do
      reg <- use registry
      SomeComponent cp <- getRegistryByUID name reg
      liftIO $ writeIORef (renderStateRef cp) st

  -- commit view changes
  use renderer >>= SDL.present

  -- wait
  SDL.delay 33

  q <- use isQuit
  unless q $ mainloop root

  SDLF.quit
  SDL.quit

-- | Watcher function
watch :: (Component UI src, Component UI tgt) => ComponentView src -> (RenderState -> Signal src -> StateT (Model tgt) UI ()) -> Watcher UI tgt
watch = Watcher

-- | Add a watch signal
addWatchSignal :: Component UI tgt => ComponentView tgt -> Watcher UI tgt -> UI ()
addWatchSignal cp (w@(Watcher srcCp callback)) = do
  let src = name srcCp
  d <- use distributer
  when (src `M.notMember` d) $ distributer %= M.insert src []
  distributer . ix src %= (:) (SomeCallback (name cp) callback)

instance Component UI "raw" where
  data Model "raw" = RawModel Graphical
  type ModelParam "raw" = ()
  data Signal "raw"

  newModel () = return $ RawModel empty

  getGraphical (RawModel g) = return g

-- | Register a graphical object to raw component
rawGraphical :: ComponentView "raw" -> Graphical -> ComponentView "raw"
rawGraphical cp g = cp { model = RawModel g }

fromModel :: Component UI a => Model a -> UI (ComponentView a)
fromModel model = do
  uniq <- liftIO newUnique
  renderRef <- liftIO $ newIORef defRenderState

  return $ ComponentView
    { model = model
    , name = prefix model ++ show (hashUnique uniq)
    , renderStateRef = renderRef
    }

-- | Get the view with current snapshot of model
view :: (Component UI a) => ComponentView a -> UI Graphical
view cp = do
  r <- use registry
  SomeComponent cp <- getRegistryByUID (name cp) r
  viewInfo (name cp) <$> getGraphical (model cp)

-- | Modify internal model of a component
operateModel :: Component UI a => ComponentView a -> StateT (Model a) UI () -> UI ()
operateModel cp f = do
  r <- use registry
  modifyMRegistryByUID (name cp) r $ \(SomeComponent cp) -> do
    model' <- flip execStateT (model cp) $ unsafeCoerce f
    return $ SomeComponent $ cp { model = model' }

-- | Constructor of a component
new :: Component UI a => ModelParam a -> UI (ComponentView a)
new p = do
  cp <- fromModel =<< newModel p
  initComponent cp
  return cp

