-- xmonad.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


import           XMonad
import           XMonad.Actions.CycleWS       (nextWS, prevWS)
import           XMonad.Actions.NoBorders     (toggleBorder)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks     (avoidStruts, manageDocks, docksEventHook)
import           XMonad.Hooks.ManageHelpers   (doFullFloat, isDialog,
                                               isFullscreen)
import           XMonad.Hooks.SetWMName       (setWMName)
import           XMonad.Hooks.UrgencyHook     (UrgencyHook (..), focusUrgent,
                                               withUrgencyHook)
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile  (MirrorResize (..),
                                               ResizableTall (..))
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         (additionalKeysP)
import           XMonad.Util.NamedWindows     (getName)
import           XMonad.Util.Scratchpad       (scratchpadManageHook,
                                               scratchpadSpawnActionTerminal)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

import           Control.Applicative          ((<$>))
import           Data.List                    (isInfixOf, (\\))
import           Data.Monoid                  (All (..))

-- The main logger class.
mainLogger :: String
mainLogger = "XMonad"

-- The official "Focused UI Element" color.
currentWindowColor :: String
currentWindowColor = "#aa2e00"

urgentFg :: String
urgentFg = "#dc143c"

urgentBg :: String
urgentBg = "#f1f227"

myBar :: String
myBar = "xmobar"

myTerminal :: String
myTerminal = "urxvtcd"

myPP :: PP
myPP = xmobarPP { ppCurrent = xmobarColor currentWindowColor "" . wrap "<" ">"
                , ppTitle = xmobarColor currentWindowColor "" . shorten 60
                , ppSep = " | "
                , ppHidden = noScratchPad
                , ppUrgent = xmobarColor urgentFg urgentBg . pad
                }
  where
    noScratchPad ws = if "NSP" `isInfixOf` ws then "" else ws

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
  where toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape $
               ["1","2","3","4","5","6","7","8","9"]
  where clickable l = [ "<action=xdotool key Super+" ++ show n ++ ">"
                        ++ ws ++ "</action>"
                      | (i,ws) <- zip [1..9] l, let n = i]
        xmobarEscape = (=<<) doubleLts
          where doubleLts '<' = "<<"
                doubleLts x = [x]

myClientMask :: EventMask
myClientMask = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask
               .|. focusChangeMask

-- Main configuration, override the defaults to your liking
myConfig = ewmh defaultConfig
  { terminal = myTerminal -- urxvt config in ~/.Xresources
  , manageHook = myManageHooks
  , startupHook = setWMName "LG3D" -- What is this for?
  , layoutHook = smartBorders $ avoidStruts myLayoutHook
  , handleEventHook = mconcat
    [ docksEventHook
    , handleEventHook defaultConfig <+> fullscreenEventHook
    ]
  , focusedBorderColor = currentWindowColor
  , logHook = dynamicLogWithPP myPP
  , modMask = mod4Mask -- Rebind Mod to the Windows key
  , focusFollowsMouse = False
  , borderWidth = 4
  , workspaces = myWorkspaces
  } `additionalKeysP`
  [ ("M4-S-z", spawn "xscreensaver-command -lock")
  , ("C-<Print>", spawn "sleep 0.2; scrot -s")
  , ("<Print>", spawn "scrot")
  , ("M4-a", sendMessage MirrorShrink)
  , ("M4-z", sendMessage MirrorExpand)
  , ("M4-`", scratchpadSpawnActionTerminal myTerminal)
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
  , ("<XF86AudioMute>", spawn "amixer set Master toggle")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -10")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight +10")
  , ("<XF86KbdBrightnessDown>", spawn "kbdlight down")
  , ("<XF86KbdBrightnessUp>", spawn "kbdlight up")
  , ("M4-u", focusUrgent)
  , ("M4-<Left>", prevWS)
  , ("M4-<Right>", nextWS)
  , ("M4-g", withFocused toggleBorder)
  , ("M4-p", spawn $ "j4-dmenu-desktop --term zsh --dmenu=\"dmenu -i -b -p 'exec '"
             ++ " -nb '#dac7b3' -nf black -sf black -sb '#e1f1f6' -fn"
             ++ " 'inconsolatazi4-13:antialias=true:bold'"
             ++ " && eval \" exec $exe\"\"")
  ]
  where myLayoutHook = ResizableTall 1 (3/100) (1/2) []
                       ||| Tall 1 (3/100) (1/2)
                       ||| Mirror (Tall 1 (3/100) (1/2))
                       ||| Full
        myManageHooks = composeAll
                        [ isFullscreen --> doFullFloat
                        , isDialog --> doF W.shiftMaster <+> doF W.swapDown
                        , manageDocks
                        , manageScratchPad
                        ]
        manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
          where h = 1  -- height, 100%
                w = 0.42  -- width, 42%
                t = 0  -- distance from top, 0%
                l = 0  -- distance from left, 0%
