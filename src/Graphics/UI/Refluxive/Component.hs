{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
module Graphics.UI.Refluxive.Component
  ( Component(..)
  , ComponentView(..)
  , Watcher(..)
  , getModel
  ) where

import qualified SDL as SDL
import SDL.Vect
import Graphics.UI.Refluxive.Graphical
import Control.Monad.State
import Data.IORef
import GHC.TypeLits (symbolVal, KnownSymbol)

type EventStream a = IORef [a]

data Watcher m tgt = forall src. (Component m src, Component m tgt) => Watcher (ComponentView src) (Signal src -> StateT (Model tgt) m ())

data ComponentView a
  = ComponentView
  { model :: Model a
  , name :: String
  }

getModel :: ComponentView a -> Model a
getModel cp = model cp

class KnownSymbol a => Component m a | a -> m where
  type family ModelParam a
  data family Model a
  data family Signal a

  uid :: proxy a -> String
  uid = symbolVal

  watcher :: ComponentView a -> [Watcher m a]
  watcher _ = []

  newModel :: MonadIO m => ModelParam a -> m (Model a)

  initComponent :: MonadIO m => ComponentView a -> m ()
  initComponent _ = return ()

  getGraphical :: MonadIO m => Model a -> m Graphical


