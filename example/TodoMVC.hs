{-# LANGUAGE OverloadedStrings #-}
module TodoMVC where

import qualified SDL as SDL
import SDL.Vect
import Control.Monad
import Control.Monad.State
import Data.Extensible
import qualified Data.Text as T
import Graphics.UI.Lefrect

instance Component UI "text-form" where
  data Model "text-form" = TextformModel T.Text
  data Signal "text-form"

main :: IO ()
main = runUI $ do
  mainloop

