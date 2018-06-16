{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
module Graphics.UI.Refluxive.Component
  ( Component(..)
  , ComponentView(..)
  , Watcher(..)
  , getWatcherTgtUID
  ) where

import qualified SDL as SDL
import SDL.Vect
import Graphics.UI.Refluxive.Graphical
import Control.Monad.State
import Data.IORef
import GHC.TypeLits (symbolVal, KnownSymbol)

type EventStream a = IORef [a]

data Watcher m tgt = forall src. (Component m src, Component m tgt) => Watcher String (Signal src -> StateT (Model tgt) m ())

getWatcherTgtUID :: Component m tgt => Watcher m tgt -> String
getWatcherTgtUID w = uid w

data ComponentView a
  = ComponentView
  { model :: Model a
  }

class KnownSymbol a => Component m a | a -> m where
  type family ModelParam a
  data family Model a
  data family Signal a

  uid :: proxy a -> String
  uid = symbolVal

  watcher :: proxy a -> [Watcher m a]
  watcher _ = []

  newModel :: MonadIO m => ModelParam a -> m (Model a)
  getGraphical :: MonadIO m => Model a -> m Graphical


