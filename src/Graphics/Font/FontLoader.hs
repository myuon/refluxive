module Graphics.Font.FontLoader where

import qualified SDL.Font as Font
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.FileEmbed
import Network.HTTP.Simple
import Language.Haskell.TH
import System.Directory

-- | Load webfont and embed the ttf content as ByteString at compile-time
loadFont
  :: String -- ^ font URL
  -> Q Exp
loadFont url = do
  let path = ".stack-work/refluxive/" ++ fmap (\c -> if c `elem` ("/:" :: String) then '_' else c) url

  liftIO $ do
    createDirectoryIfMissing True ".stack-work/refluxive"

    b <- doesFileExist path
    when (not b) $ do
      res <- httpBS $ parseRequest_ url
      BS.writeFile path $ getResponseBody res

  embedFile path

