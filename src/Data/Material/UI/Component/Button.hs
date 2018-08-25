module Data.Material.UI.Component.Button where

import qualified SDL.Primitive as SDL
import SDL.Vect
import Control.Lens
import Graphics.UI.Refluxive

instance Component UI "button" where
  type ModelParam "button" = ()

  data Model "button" = ButtonModel

  data Signal "button" = Click

  newModel _ = return ButtonModel

  initComponent self = return ()

  getGraphical model = do
    let label = text "roboto" 14 "BUTTON"
    labelSize <- textSize "roboto" 14 "BUTTON"

    return $ graphics $
      [ colored (V4 0x62 0x00 0xee 0xff) $ rectangleWith (def { rounded = Just 2 }) (V2 0 0) (V2 (toEnum $ labelSize^._x + 16 * 2) 36)
      , colored (V4 0xff 0xff 0xff 0xff) $ translate (V2 16 (toEnum $ 18 - labelSize^._y `div` 2)) label
      ]

