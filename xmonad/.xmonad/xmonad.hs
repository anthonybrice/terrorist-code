-- xmonad.hs

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.List                      (isInfixOf)

import           XMonad
import           XMonad.Actions.CycleWS         (nextWS, prevWS)
import           XMonad.Actions.NoBorders       (toggleBorder)
import           XMonad.Config.Desktop          (desktopConfig)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops      (ewmh, fullscreenEventHook)
import XMonad.Layout.Fullscreen (fullscreenManageHook)
import           XMonad.Hooks.ManageDocks       (avoidStruts, docksEventHook,
                                                 manageDocks)
import           XMonad.Hooks.ManageHelpers     (doFullFloat, isDialog,
                                                 isFullscreen)
import           XMonad.Hooks.SetWMName         (setWMName)
import           XMonad.Hooks.UrgencyHook       (focusUrgent)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile    (MirrorResize (..),
                                                 ResizableTall (..))
import qualified XMonad.StackSet                as W
import           XMonad.Util.EZConfig           (additionalKeysP)
import           XMonad.Util.Scratchpad         (scratchpadManageHook,
                                                 scratchpadSpawnActionTerminal)
import           XMonad.Util.Themes

import           XMonad.Layout.Decoration
import           XMonad.Layout.SimpleDecoration (shrinkText, simpleDeco)

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
myTerminal = "urxvtc"

myPP :: PP
myPP = xmobarPP { ppCurrent = xmobarColor currentWindowColor "" . wrap "(" ")"
                , ppTitle = xmobarColor currentWindowColor "" . shorten 90
                , ppSep = " | "
                , ppHidden = noScratchPad
                , ppUrgent = xmobarColor urgentFg urgentBg . pad
                }
  where
    noScratchPad ws = if "NSP" `isInfixOf` ws then "" else ws
    --notitle x = ""

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
                doubleLts x   = [x]

-- Main configuration, override the defaults to your liking
myConfig = ewmh desktopConfig
  { terminal = myTerminal -- urxvt config in ~/.Xresources
  , manageHook = myManageHooks
  , startupHook = setWMName "LG3D" -- this fixes xmonad startup
  , layoutHook = myLayoutHook
  , handleEventHook = mconcat
    [ docksEventHook
    , handleEventHook def <+> fullscreenEventHook
    ]
  , focusedBorderColor = currentWindowColor
  , logHook = dynamicLogWithPP myPP
  , modMask = mod4Mask -- Rebind Mod to the Windows key
  , focusFollowsMouse = False
  , clickJustFocuses = False
  , borderWidth = 4
  , workspaces = myWorkspaces
  } `additionalKeysP`
  [ ("C-<Print>", spawn "sleep 0.2; scrot -s")
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
  , ("<XF86HomePage>", windows $ W.greedyView "1")
  , ("M4-u", focusUrgent)
  , ("M4-<Left>", prevWS)
  , ("M4-<Right>", nextWS)
  , ("M4-g", withFocused toggleBorder)
  , ("M4-p", spawn j4String)
  , ("<XF86LaunchB>", spawn j4String)
  ]
  where
    myManageHooks = composeAll
                    [ isFullscreen --> doFullFloat
                    , isDialog --> doF W.shiftMaster <+> doF W.swapDown
                    , manageDocks
                    , manageScratchPad
                    , fullscreenManageHook
                    ]
    manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
      where
        h = 1  -- height, 100%
        w = 0.42  -- width, 42%
        t = 0  -- distance from top, 0%
        l = 0  -- distance from left, 0%

    myLayoutHook =
      smartBorders $ avoidStruts layouts

    theme = def
            { activeColor = currentWindowColor
            , decoWidth = 1152
            , decoHeight = 14
            }

    layouts =
      ResizableTall 1 (3/100) (1/2) []
      ||| Mirror (Tall 1 (3/100) (1/2))
      ||| Full

    j4String =
      "j4-dmenu-desktop --term zsh --dmenu=\"dmenu -i -b -p 'exec '"
             ++ " -nb '#dac7b3' -nf black -sf black -sb '#e1f1f6' -fn"
             ++ " 'inconsolata-13:antialias=true:bold'"
             ++ " && eval \" exec $exe\"\""
