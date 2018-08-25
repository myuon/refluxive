module DevServer where

import SDL.Vect
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.List
import qualified Language.Haskell.Interpreter as Hint
import System.FSNotify
import Graphics.UI.Refluxive
import qualified Data.Material.UI.Component.Button as Button

hotreload :: String -> UI ()
hotreload _ = replace $ \name comp ->
  if "button" `isPrefixOf` name
  then fmap SomeComponent $ Hint.interpret "new @\"button\" ()" (Hint.as :: ComponentView "button")
  else return comp

runner :: MVar (Maybe String) -> IO ()
runner ref = runUI $ do
  setClearColor (V4 240 240 240 255)
  btn <- new @"button" ()
  register btn

  mainloopDev (Just (ref, hotreload)) [asRoot btn]

main = withManager $ \mgr -> do
  let entry = "example/MaterialUI.hs"
  putStrLn "start watching..."

  ref <- newMVar Nothing
  forkIO $ runner ref
  watchTree mgr "example/" (const True) (\_ -> putMVar ref (Just "button"))

  forever $ threadDelay 1000000

{-
runUI $ do
  setClearColor (V4 240 240 240 255)

  btn <- new @"button" ()
  register btn

  mainloop [asRoot btn]
-}



