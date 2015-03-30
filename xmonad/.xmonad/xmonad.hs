-- xmonad.hs

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.EZConfig       (additionalKeysP)

-- For quake-like terminal
import           XMonad.Util.Scratchpad

import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders
import XMonad.Actions.NoBorders
import qualified XMonad.StackSet            as W

-- For notifications
import           Control.Applicative        ((<$>))
import Control.Monad ((>=>))
import           XMonad.Hooks.UrgencyHook
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run

-- For resizable windows
import XMonad.Layout.ResizableTile

-- For cycling through workspaces
import XMonad.Actions.CycleWS

import XMonad.Hooks.FadeInactive

import Data.List (isInfixOf)
import Data.List.Utils (replace)

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

-- Custom PP, configure it as you like. It determines what is being
-- written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor currentWindowColor "" . wrap "<" ">"
                , ppTitle = xmobarColor currentWindowColor "" . shorten 48
                , ppSep = " | "
                --, ppHiddenNoWindows = namedOnly
                , ppHidden = noScratchPad
                , ppUrgent = xmobarColor "#dc143c" "#f1f227" . pad
                }
  where
    noScratchPad ws = if "NSP" `isInfixOf` ws then "" else ws
    -- 'namedOnly' is broken because of the action tags in every ws
    --namedOnly ws = if any (`elem` ws) ['a' .. 'z'] then pad ws else ""

-- Key bindings to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myLayoutHook = ResizableTall 1 (3/100) (1/2) []
               ||| Tall 1 (3/100) (1/2)
               ||| Mirror (Tall 1 (3/100) (1/2))
               ||| Full

myTerminal = "urxvtc"

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape $ ["1","2","3","4","5","6","7","8","9"]
  where clickable l = [ "<action=xdotool key Super+" ++ show n ++ ">"
                        ++ ws ++ "</action>"
                      | (i,ws) <- zip [1..9] l, let n = i]

-- Main configuration, override the defaults to your liking
myConfig = withUrgencyHook LibNotifyUrgencyHook $ defaultConfig
  { terminal = myTerminal -- urxvt config in ~/.Xresources
  , manageHook = myManageHooks
  , startupHook = setWMName "LG3D"
  , layoutHook = smartBorders $ avoidStruts myLayoutHook
  , focusedBorderColor = currentWindowColor
  , logHook = do
    dynamicLogWithPP myPP
    --fadeInactiveLogHook 0.2
  , modMask = mod4Mask -- Rebind Mod to the Windows key
  , focusFollowsMouse = False
  , borderWidth = 3
  , workspaces = myWorkspaces
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
  , ("M4-u", focusUrgent)
  , ("M4-<Left>", prevWS)
  , ("M4-<Right>", nextWS)
  , ("M4-g", withFocused toggleBorder)
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
    h = 1 -- terminal height, 100%
    w = 0.4 -- terminal width, 40%
    t = 0 -- distance from top edge, 0%
    l = 0 -- distance from left edge, 0%
