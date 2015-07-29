import Control.Monad (liftM)
import Data.Ratio ((%))
import System.IO

import XMonad hiding ((|||))

import XMonad.Actions.Volume

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.DwmStyle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed

import qualified XMonad.StackSet as W
import XMonad.Util.Dzen
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

-- in ~/.xmonad/lib
import Music.Pithos

myWorkspaces = ["1:emacs", "2:web", "3:im"] ++ map show [4..9]

myTheme = defaultTheme { decoHeight = 16
                       , activeColor = "#a6c292"
                       , activeBorderColor = "#a6c292"
                       , activeTextColor = "#000000"
                       , inactiveBorderColor = "#000000"
                       }

myLayouts = avoidStruts $ smartBorders $
  onWorkspace "1:emacs" (Mirror tiled ||| tiled ||| tabs) $
  onWorkspace "2:web" (tabs) $
  onWorkspace "3:im" (named "IM" (reflectHoriz $ withIM (1%8) (Title "Buddy List")
                       (reflectHoriz $ dwmStyle shrinkText myTheme $ spacing 4 $ withBorder 3 $ grid))) $
  standardLayouts
    where
      grid = named "Grid" (GridRatio (3/2) ||| GridRatio (2/3))
      standardLayouts = (tiled ||| Mirror tiled ||| tabs)
      tiled = named "Tall" (ResizableTall 1 (3/100) (11/16) [])
      tabs = named "Tabs" (tabbed shrinkText myTheme)

myManageHook = composeAll [ className =? "Emacs"  --> doF (W.shift "1:emacs")
                          , className =? "Google-chrome" --> doF (W.shift "2:web")
                          , className =? "Pidgin" --> doF (W.shift "3:im")
                          ]

alert :: String -> X ()
alert = dzenConfig centered
centered = onCurr (center 280 66)
           >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"

isMute :: Bool -> String
isMute True = "Mute"
isMute False = "Unmute"

main = do
     xmproc <- spawnPipe "xmobar"
     xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
         { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
         , layoutHook = myLayouts
         , logHook = dynamicLogWithPP xmobarPP
                         { ppOutput = hPutStrLn xmproc
                         , ppTitle = xmobarColor "green" "" . shorten 50
                         , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                         }
         , modMask = mod4Mask
         , focusFollowsMouse = False
         , startupHook = setWMName "LG3D"
         , workspaces = myWorkspaces
         } `additionalKeysP`
          [("M-a",      pithosSelect)
          ,("M-S-l",    spawn "xscreensaver-command -lock")
          ,("<F9>",     toggleMute >>= (alert . isMute))
          ,("<F10>",    (liftM round . lowerVolume) 3 >>= (alert . show))
          ,("<F11>",    (liftM round . raiseVolume) 3 >>= (alert . show))
          ]
