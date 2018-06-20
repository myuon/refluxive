{-|
  UI Components
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
module Graphics.UI.Refluxive.Component
  ( Component(..)
  , ComponentView(..)
  , Watcher(..)
  , getModel
  , prefix
  ) where

import qualified SDL as SDL
import SDL.Vect
import Graphics.UI.Refluxive.Graphical
import Control.Monad.State
import Data.IORef
import GHC.TypeLits (symbolVal, KnownSymbol)

type EventStream a = IORef [a]

-- | Watcher type with context @m@ and target ui name @tgt@
data Watcher m tgt = forall src. (Component m src, Component m tgt) =>
  Watcher (ComponentView src) (RenderState -> Signal src -> StateT (Model tgt) m ())

-- | An instance for Component
data ComponentView a
  = ComponentView
  { model :: Model a
  , name :: String
  , renderStateRef :: IORef RenderState
  }

-- | Get current model
getModel :: ComponentView a -> Model a
getModel cp = model cp

prefix :: Component m a => proxy a -> String
prefix = symbolVal

-- | Component type
class KnownSymbol a => Component m a | a -> m where
  -- | Parameter, passing to the constructor
  type family ModelParam a

  -- | Model datatype
  data family Model a

  -- | Signal datatype
  data family Signal a

  -- | Creating new model from 'ModelParam', it is called internally in new function
  newModel :: MonadIO m => ModelParam a -> m (Model a)

  -- | Initialize function, it is called internally after new function
  initComponent :: MonadIO m => ComponentView a -> m ()
  initComponent _ = return ()

  -- | Projecting model to view
  getGraphical :: MonadIO m => Model a -> m Graphical


