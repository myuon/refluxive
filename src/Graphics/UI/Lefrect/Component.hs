module Graphics.UI.Lefrect.Component
  ( Component(..)
  , ComponentView(..)
  , new
  , on
  , emit
  , create
  , view
  , exec
  , rawGraphical
  ) where

import qualified SDL as SDL
import SDL.Vect
import Graphics.UI.Lefrect.Graphical
import Control.Monad.State
import Data.IORef
import Data.Ix

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

    SDL.addEventWatch $ \case
      SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ _ _ SDL.ButtonLeft 1 (P pos))) -> do
        when (inRange (V2 0 0, V2 200 100) pos) $ do
          print "click signal"
          emit cp Clicked
      _ -> return ()

    return $ CounterView $ cp `on` \case
      Clicked -> do
        CounterModel c <- get
        modify $ \(CounterModel n) -> CounterModel (n+1)
        lift $ putStrLn $ "counter:" ++ show c

  getComponentView (CounterView x) = x

  getGraphical (CounterModel n) = do
    return $ graphics
      [ colored (V4 100 200 255 255) $ rectangle (V2 0 0) (V2 2 1)
      ]

instance Component "raw" where
  data View "raw" = RawView (ComponentView "raw")
  data Model "raw" = RawModel Graphical
  data Signal "raw"

  setup = do
    cp <- liftIO $ new $ RawModel empty
    return $ RawView cp

  getComponentView (RawView x) = x

  getGraphical (RawModel g) = return g

rawGraphical :: View "raw" -> Graphical -> View "raw"
rawGraphical (RawView cp) g = RawView $ cp { model = RawModel g }

create :: (MonadIO m, Component a) => m (ComponentView a)
create = fmap getComponentView setup

view :: (MonadIO m, Component a) => ComponentView a -> m Graphical
view cp = getGraphical (model cp)

exec :: (MonadIO m, Component a) => ComponentView a -> m (ComponentView a)
exec cp = liftIO $ fmap (\m -> cp { model = m }) $ (`execStateT` (model cp)) $ do
  events <- liftIO $ readIORef $ event cp
  mapM_ (\ev -> callbacks cp ev) events
  liftIO $ writeIORef (event cp) []

