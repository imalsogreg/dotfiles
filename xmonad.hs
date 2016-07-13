module Main where

import qualified Data.Map as M
import XMonad
import XMonad.Actions.KeyRemap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import System.IO

main :: IO ()
main = do
  xmproc <- spawnPipe "/run/current-system/sw/bin/xmobar /home/greghale/.xmobarrc"
  xmonad $ defaultConfig
    { borderWidth = 2
    , focusedBorderColor = "#0d9bcc"
    , keys = \c -> myKeys c `M.union` (keys defaultConfig c)
    , layoutHook = avoidStruts $   layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle  = xmobarColor "green" "" . shorten 50
                  }
    , manageHook = manageDocks <+> manageHook defaultConfig
    , normalBorderColor = "#cccccc"
    , startupHook = myStartupHook
    , terminal    = "xfce4-terminal"
    }

myStartupHook = setWMName "LG3D"

myWorkspaces = map show [1..9]
myNumRow = [xK_ampersand
           , xK_bracketleft
           , xK_braceleft
           , xK_braceright
           , xK_parenleft
           , xK_equal
           , xK_asterisk
           , xK_parenright
           , xK_plus] 

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
  M.fromList $ [((m.|. modm, k), windows $ f i)
             | (i,k) <- zip (XMonad.workspaces conf) myNumRow
             , (f,m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
