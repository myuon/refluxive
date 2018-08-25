module Graphics.UI.LoadFontHack where

import qualified Graphics.Font.FontLoader as FontLoader
import Control.Monad.IO.Class
import Language.Haskell.TH

-- | Load font with name & web font url. This ExpQ will generate UI action
loadFont
  :: String  -- ^ font name (on your choice)
  -> String  -- ^ font URL
  -> Q Exp
loadFont name url = [| loadFontUI name $(FontLoader.loadFont url) |]

