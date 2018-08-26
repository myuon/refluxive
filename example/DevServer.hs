module DevServer where

import SDL.Vect
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.List
import qualified Language.Haskell.Interpreter as Hint
import System.FSNotify
import Graphics.UI.Refluxive
import qualified Data.Material.UI.Component.Button as Button

hotreload :: String -> UI ()
hotreload _ = do
  liftIO $ putStrLn "reloading..."

  result <- liftIO $ Hint.runInterpreter $ do
    Hint.set [Hint.languageExtensions Hint.:= [Hint.TypeApplications, Hint.DataKinds, Hint.PolyKinds, Hint.TypeFamilies]]
    Hint.reset
    Hint.setImports ["Graphics.UI.Refluxive", "Data.Material.UI.Component.Button"]
    Hint.interpret "getGraphical @_ @\"button\"" (Hint.as :: Model "button" -> UI Graphical)
  case result of
    Right r -> replace "button" r
    Left err -> liftIO $ print err

runner :: MVar String -> IO ()
runner ref = runUI $ do
  setClearColor (V4 240 240 240 255)
  btn <- new @"button" ()
  register btn

  mainloopDev (Just (ref, hotreload)) [asRoot btn]

main = withManager $ \mgr -> do
  putStrLn "start watching..."

  ref <- newEmptyMVar
  forkIO $ runner ref
  watchTree mgr "example" (const True) (\e -> print e >> putMVar ref "button")
  watchTree mgr "src" (const True) (\e -> print e >> putMVar ref "button")

  forever $ threadDelay 1000000

