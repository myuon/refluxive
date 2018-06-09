module Component
  ( Component(..)
  , new
  , on
  , emit
  , create
  , view
  ) where

import Graphical
import Control.Monad.State
import Data.IORef
import Linear.V2
import Linear.V4

type EventStream a = IORef [a]

class Component a where
  data family View a
  data family Model a
  data family Signal a

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

instance Component "counter" where
  data View "counter" = CounterView (ComponentView "counter")
  data Model "counter" = CounterModel Int
  data Signal "counter" = Clicked

  setup = do
    cp <- liftIO $ new (CounterModel 0)
    return $ CounterView $ cp `on` \case
      Clicked -> do
        CounterModel c <- get
        lift $ putStrLn $ "counter:" ++ show c

  getComponentView (CounterView x) = x

  getGraphical (CounterModel n) = do
    return $ graphics
      [ colored (V4 100 200 255 255) $ rectangle (V2 0 0) (V2 2 1)
      ]

create :: (MonadIO m, Component a) => m (ComponentView a)
create = fmap getComponentView setup

view :: (MonadIO m, Component a) => ComponentView a -> m Graphical
view cp = getGraphical (model cp)

