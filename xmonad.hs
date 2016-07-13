module Main where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import System.IO

main :: IO ()
main = do
  xmproc <- spawnPipe "/run/current-system/sw/bin/xmobar /home/greghale/.xmobarrc"
  xmonad $ defaultConfig
    { borderWidth = 2
    , terminal    = "xfce4-terminal"
    , normalBorderColor = "#cccccc"
    , focusedBorderColor = "#0d9bcc"
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts $   layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle  = xmobarColor "green" "" . shorten 50
                  }
    }
