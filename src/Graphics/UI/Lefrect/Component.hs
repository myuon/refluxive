{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
module Graphics.UI.Lefrect.Component
  ( Component(..)
  , ComponentView(..)
  , Watcher(..)
  , getWatcherTgtUID
  , new
  , on
  , create
  , view
  , exec
  ) where

import qualified SDL as SDL
import SDL.Vect
import Graphics.UI.Lefrect.Graphical
import Control.Monad.State
import Data.IORef
import GHC.TypeLits (symbolVal, KnownSymbol)

type EventStream a = IORef [a]

data Watcher m tgt = forall src. (Component m src, Component m tgt) => Watcher String (Signal src -> StateT (Model tgt) m ())

getWatcherTgtUID :: Component m tgt => Watcher m tgt -> String
getWatcherTgtUID w = uid w

class KnownSymbol a => Component m a | a -> m where
  data family View a
  data family Model a
  data family Signal a

  uid :: proxy a -> String
  uid = symbolVal

  watcher :: proxy a -> [Watcher m a]
  watcher _ = []

  setup :: MonadIO m => m (View a)
  getComponentView :: View a -> ComponentView a
  getGraphical :: MonadIO m => Model a -> m Graphical

data ComponentView a
  = ComponentView
  { event :: EventStream (Signal a)
  , callbacks :: Signal a -> StateT (Model a) IO ()
  , model :: Model a
  }

new :: Model a -> IO (ComponentView a)
new model = do
  stream <- newIORef []

  return $ ComponentView
    { event = stream
    , callbacks = \_ -> return ()
    , model = model
    }

on :: ComponentView a -> (Signal a -> StateT (Model a) IO ()) -> ComponentView a
on cp callback = cp { callbacks = \signal -> callback signal >> callbacks cp signal }

emit :: ComponentView a -> Signal a -> IO ()
emit cp s = modifyIORef (event cp) (s:)

create :: (MonadIO m, Component m a) => m (ComponentView a)
create = fmap getComponentView setup

view :: (MonadIO m, Component m a) => ComponentView a -> m Graphical
view cp = getGraphical (model cp)

exec :: (MonadIO m, Component m a) => ComponentView a -> m (ComponentView a)
exec cp = liftIO $ fmap (\m -> cp { model = m }) $ (`execStateT` (model cp)) $ do
  events <- liftIO $ readIORef $ event cp
  mapM_ (\ev -> callbacks cp ev) events
  liftIO $ writeIORef (event cp) []

