{-# LANGUAGE CPP #-}
module Data.Material.UI.Foundation.Color where

import qualified SDL.Primitive as SDL

#define COLOR(p,v) pattern p :: SDL.Color; pattern p = v

COLOR(Red50, 0xffebee)
COLOR(Red100, 0xffcdd2)
COLOR(Red200, 0xef9a9a)
COLOR(Red300, 0xe57373)
COLOR(Red400, 0xef5350)
COLOR(Red500, 0xf44336)
COLOR(Red600, 0xe53935)
COLOR(Red700, 0xd32f2f)
COLOR(Red800, 0xc62828)
COLOR(Red900, 0xb71c1c)
COLOR(RedA100, 0xff8a80)
COLOR(RedA200, 0xff5252)
COLOR(RedA300, 0xff1744)
COLOR(RedA400, 0xd50000)

COLOR(Purple50, 0xf3e5f5)
COLOR(Purple100, 0xe1bee7)
COLOR(Purple200, 0xce93d8)
COLOR(Purple300, 0xba68c8)
COLOR(Purple400, 0xab47bc)
COLOR(Purple500, 0x9c27b0)
COLOR(Purple600, 0x8e24aa)
COLOR(Purple700, 0x7b1fa2)
COLOR(Purple800, 0x6a1b9a)
COLOR(Purple900, 0x4a148c)
COLOR(PurpleA100, 0xea80fc)
COLOR(PurpleA200, 0xe040fb)
COLOR(PurpleA300, 0xd500f9)
COLOR(PurpleA400, 0xaa00ff)

