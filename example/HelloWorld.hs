{-# LANGUAGE OverloadedStrings #-}
module HelloWorld where

import qualified SDL as SDL
import SDL.Vect
import Control.Monad
import Control.Monad.State
import Control.Lens (use)
import Data.Ix (inRange)
import Graphics.UI.Refluxive

instance Component UI "counter" where
  type ModelParam "counter" = ()
  data Model "counter" = CounterModel Int
  data Signal "counter" = Clicked

  newModel () = return (CounterModel 0)

  initComponent self = do
    b <- use _builtIn

    addWatchSignal self $ watch b $ \_ -> \case
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (P pos)))) -> do
        when (inRange (V2 0 0, V2 200 100) pos) $ lift $ emit self Clicked
        liftIO $ print "builtin watch!"
      _ -> return ()
    addWatchSignal self $ watch self $ \_ -> \case
      Clicked -> do
        CounterModel c <- get
        modify $ \(CounterModel n) -> CounterModel (n+1)
        liftIO $ putStrLn $ "counter:" ++ show c
        return ()

  getGraphical (CounterModel n) = do
    return $ graphics
      [ colored (V4 100 200 255 255) $ rectangle (V2 0 0) (V2 200 100)
      ]

main :: IO ()
main = runUI $ do
  setClearColor (V4 240 240 240 255)

  counter <- new @"counter" ()
  register counter

  raw <- new @"raw" ()
  rawGraphical raw $ graphics $
    [ gridLayout (V2 50 50) $ translate (V2 2 2) $ graphics $
      [ colored (V4 30 30 30 255) $ graphics $
        [ rectangle (V2 0 0) (V2 1 1)
        , rectangle (V2 1 1) (V2 1 1)
        ]
      , colored (V4 255 255 255 255) $ graphics $
        [ rectangle (V2 1 0) (V2 1 1)
        , rectangle (V2 0 1) (V2 1 1)
        ]
      , colored (V4 200 100 100 255) $ rectangleWith (def { fill = False }) (V2 4 4) (V2 1 1)
      , colored (V4 200 100 100 255) $ rectangleWith (def { rounded = Just 10 }) (V2 5 5) (V2 1 1)
      , colored (V4 255 0 0 255) $ textDef "Hello, World!"
      ]
    , clip (V2 200 40) $ colored (V4 0 0 0 255) $ textDef "hey youyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"
    ]

  mainloop [asRoot raw, asRoot counter]

