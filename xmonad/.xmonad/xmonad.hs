import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
-- import XMonad.Layout.NoBorders
-- import XMonad.Util.Run(spawnPipe)
import           XMonad.Util.EZConfig       (additionalKeysP)
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
                , ppTitle = xmobarColor currentWindowColor "" . shorten 72
                , ppSep = " | "
                }

-- Key bindings to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

--myLayoutHook = Full ||| noBorders Full

-- Main configuration, override the defaults to your liking
myConfig = withUrgencyHook LibNotifyUrgencyHook $ defaultConfig
  { terminal = "gnome-terminal" -- urxvt config in ~/.Xresources
  , manageHook = myManageHooks
  , startupHook = setWMName "LG3D"
  , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
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
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 10%-")
  , ("<XF86AudioMute>", spawn "amixer set Master toggle")
  ]


myManageHooks = composeAll
  [ isFullscreen --> doFullFloat
  , isDialog --> doF W.shiftMaster <+> doF W.swapDown
  -- , className =? "NetBeans IDE 8.0" --> doFloat
  -- , className =? "MATLAB" --> doFloat
  -- , className =? "com-mathworks-util-PostVMInit" --> doFloat
  -- , className =? "jetbrains-idea" --> doFloat
  , manageDocks
  ]

-- myLogHook h = dynamicLogWithPP $ xmobarPP
--               { ppTitle = xmobarColor "green" "" . shorten 50
--               , ppOutput = hPutStrLn h
--               }
