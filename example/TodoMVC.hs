{-# LANGUAGE OverloadedStrings #-}
module TodoMVC where

import qualified SDL as SDL
import SDL.Vect
import Control.Monad
import Control.Monad.State
import Data.Extensible
import qualified Data.Text as T
import Graphics.UI.Refluxive

instance Component UI "text-form" where
  data Model "text-form" = TextformModel T.Text
  data Signal "text-form" = TextInput

  watcher _ =
    [ watch "builtin" $ \case
        BuiltInSignal (SDL.Event _ (SDL.TextInputEvent (SDL.TextInputEventData _ txt))) -> do
          modify $ (\(TextformModel t) -> TextformModel $ t `T.append` txt)
        _ -> return ()
    ]

  setup = do
    cp <- liftIO $ new (TextformModel "")
    return cp

  getGraphical (TextformModel txt) = do
    return $ graphics
      [ colored (V4 40 40 40 255) $ rectangleWith (#fill @= False <: nil) (V2 0 0) (V2 200 100)
      , translate (V2 20 20) $ text txt
      ]

main :: IO ()
main = runUI $ do
  textform <- setup @_ @"text-form"
  register textform

  mainloop

