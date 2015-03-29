-- xmonad.hs

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
-- import XMonad.Layout.NoBorders
-- import XMonad.Util.Run(spawnPipe)
import           XMonad.Util.EZConfig       (additionalKeysP)
import           XMonad.Util.Scratchpad -- For quake-like terminal
-- import System.IO
-- import XMonad.Util.Dmenu
-- import Graphics.X11.ExtraTypes.XF86
-- import XMonad.Util.Dzen

import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet            as W

-- For notifications
import           Control.Applicative        ((<$>))
import           XMonad.Hooks.UrgencyHook
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run

-- For resizable windows
import XMonad.Layout.ResizableTile

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    Just idx <- W.findTag w <$> gets windowset

    safeSpawn "notify-send" [show name, "workspace " ++ idx]

-- Command to launch the bar.
myBar = "xmobar"

currentWindowColor = "#aa2e00"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor currentWindowColor "" . wrap "<" ">"
                , ppTitle = xmobarColor currentWindowColor "" . shorten 62
                , ppSep = " | "
                }

-- Key bindings to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myLayoutHook = ResizableTall 1 (3/100) (1/2) [] ||| Tall 1 (3/100) (1/2)
               ||| Mirror (Tall 1 (3/100) (1/2)) ||| Full

myTerminal = "urxvt"

-- Main configuration, override the defaults to your liking
myConfig = withUrgencyHook LibNotifyUrgencyHook $ defaultConfig
  { terminal = myTerminal -- urxvt config in ~/.Xresources
  , manageHook = myManageHooks
  , startupHook = setWMName "LG3D"
  , layoutHook = smartBorders $ avoidStruts $ myLayoutHook
  , focusedBorderColor = currentWindowColor
  , logHook = dynamicLogWithPP myPP
  , modMask = mod4Mask -- Rebind Mod to the Windows key
  , focusFollowsMouse = False
  , borderWidth = 3
  } `additionalKeysP`
  [ ("M4-S-z", spawn "xscreensaver-command -lock")
  , ("C-<Print>", spawn "sleep 0.2; scrot -s")
  , ("<Print>", spawn "scrot")
  , ("M4-s", spawn "synapse")
  , ("M4-p", spawn $ "exe=`dmenu_run -b -i -p 'exec ' -nb '#dac7b3' -nf black"
             ++ " -sf black -sb '#e1f1f6' -fn"
             ++ " 'inconsolatazi4-13:antialias=true:bold'"
             ++ " ` && eval \"exec $exe\"")
  , ("M4-a", sendMessage MirrorShrink)
  , ("M4-z", sendMessage MirrorExpand)
  , ("M4-`", scratchpadSpawnActionTerminal myTerminal)
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 10%-")
  , ("<XF86AudioMute>", spawn "amixer set Master toggle")
  ]


myManageHooks = composeAll
  [ isFullscreen --> doFullFloat
  , isDialog --> doF W.shiftMaster <+> doF W.swapDown
  , manageDocks
  , manageScratchPad
  ]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.1 -- terminal height, 10%
    w = 1 -- terminal width, 100%
    t = 1 - h -- distance from top edge 90%
    l = 1 - w -- distance from left edge, 0%

-- myLogHook h = dynamicLogWithPP $ xmobarPP
--               { ppTitle = xmobarColor "green" "" . shorten 50
--               , ppOutput = hPutStrLn h
--               }
