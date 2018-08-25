module Graphics.Font.FontLoader where

import qualified SDL.Font as Font
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.FileEmbed
import Network.Download
import Language.Haskell.TH
import System.Directory

-- | Load webfont and embed the ttf content as ByteString at compile-time
loadFont
  :: String -- ^ font URL
  -> Q Exp
loadFont url = do
  let path = ".stack-work/refluxive/" ++ fmap (\c -> if c == '/' then '_' else c) url

  liftIO $ do
    b <- doesFileExist path
    when (not b) $ do
      Right content <- openURI url
      BS.writeFile path content

  embedFile path

