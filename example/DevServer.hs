module DevServer where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Language.Haskell.Interpreter as Hint
import System.FSNotify

main = withManager $ \mgr -> do
  watchDir mgr "." (const True) print

  forever $ threadDelay 1000000

{-
runUI $ do
  setClearColor (V4 240 240 240 255)

  btn <- new @"button" ()
  register btn

  mainloop [asRoot btn]
-}



