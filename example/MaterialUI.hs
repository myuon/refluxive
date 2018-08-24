module MaterialUI where

import SDL.Vect
import qualified Data.Material.UI.Component.Button as Button
import Graphics.UI.Refluxive
import Data.Word

main :: IO ()
main = runUI $ do
  setClearColor (V4 240 240 240 255)

  btn <- new @"button" ()
  register btn

  mainloop [asRoot btn]


