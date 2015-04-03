-- xmonad.hs

{-# LANGUAGE OverloadedStrings #-}

import           XMonad
import           XMonad.Actions.CycleWS      (nextWS, prevWS)
import           XMonad.Actions.NoBorders    (toggleBorder)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks    (avoidStruts, manageDocks)
import           XMonad.Hooks.ManageHelpers  (doFullFloat, isDialog,
                                              isFullscreen)
import           XMonad.Hooks.SetWMName      (setWMName)
import           XMonad.Hooks.UrgencyHook    (UrgencyHook (..), focusUrgent,
                                              withUrgencyHook)
import           XMonad.Layout.NoBorders     (smartBorders)
import           XMonad.Layout.ResizableTile (MirrorResize (..),
                                              ResizableTall (..))
import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig        (additionalKeysP)
import           XMonad.Util.NamedWindows    (getName)
import           XMonad.Util.Scratchpad      (scratchpadManageHook,
                                              scratchpadSpawnActionTerminal)

import           Network.HTTP.Conduit

import           System.Log.Formatter        (simpleLogFormatter)
import           System.Log.Handler          (setFormatter)
import           System.Log.Handler.Simple   (fileHandler)
import           System.Log.Logger

import           Control.Applicative         ((<$>))
import           Data.List                   (isInfixOf)
import           Data.Monoid                 (All (..))


mainLogger :: String
mainLogger = "XMonad"

main :: IO ()
main = do
  -- Get the logger handler.
  h <- fileHandler ".xmonad/debug.log" DEBUG
       >>= \lh -> return $ setFormatter lh
                  (simpleLogFormatter "[$prio] $time | $msg")
  updateGlobalLogger mainLogger (addHandler h . setLevel DEBUG)
  debugM mainLogger "Starting XMonad."
  xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

data NotatrayUrgencyHook = NotatrayUrgencyHook deriving (Read, Show)

instance UrgencyHook NotatrayUrgencyHook where
  urgencyHook NotatrayUrgencyHook w = do
    name <- getName w
    Just idx <- W.findTag w <$> gets windowset

    -- Thanks, geekosaur! Register for focus events on this
    -- window. We'll ask notatray to clear all notifications for this
    -- window the next time it's focused.
    withDisplay $ \d -> io $ selectInput d w myClientMask

    -- POST notification to notatray.
    let initReq = parseUrl "http://localhost:3000/notification"
    case initReq of
     Nothing -> return ()
     Just req -> do
       let req' = (flip urlEncodedBody) req
                  [ ("title", "xmonad")
                  , ("content", "XMONAD")
                  , ("icon", "xmonad.png")
                  , ("action", "echo hello from xmonad")
                  ]
       _ <- io $ withManager $ httpLbs  req'
       return ()

-- Command to launch the bar.
myBar = "xmobar"

currentWindowColor = "#aa2e00"

-- Custom PP, configure it as you like. It determines what is being
-- written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor currentWindowColor "" . wrap "<" ">"
                , ppTitle = xmobarColor currentWindowColor "" . shorten 60
                , ppSep = " | "
                , ppHidden = noScratchPad
                , ppUrgent = xmobarColor "#dc143c" "#f1f227" . pad
                }
  where
    noScratchPad ws = if "NSP" `isInfixOf` ws then "" else ws

-- Key bindings to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myLayoutHook = ResizableTall 1 (3/100) (1/2) []
               ||| Tall 1 (3/100) (1/2)
               ||| Mirror (Tall 1 (3/100) (1/2))
               ||| Full

myTerminal = "urxvtcd"

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape $
               ["1","2","3","4","5","6","7","8","9"]
  where clickable l = [ "<action=xdotool key Super+" ++ show n ++ ">"
                        ++ ws ++ "</action>"
                      | (i,ws) <- zip [1..9] l, let n = i]

myClientMask :: EventMask
myClientMask = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask
               .|. focusChangeMask

-- Main configuration, override the defaults to your liking
myConfig = withUrgencyHook NotatrayUrgencyHook $ defaultConfig
  { terminal = myTerminal -- urxvt config in ~/.Xresources
  , manageHook = myManageHooks
  , startupHook = setWMName "LG3D"
  , layoutHook = smartBorders $ avoidStruts myLayoutHook
  , focusedBorderColor = currentWindowColor
  , logHook = dynamicLogWithPP myPP
  , modMask = mod4Mask -- Rebind Mod to the Windows key
  , focusFollowsMouse = False
  , handleEventHook = myHandleEventHook
  , borderWidth = 3
  , workspaces = myWorkspaces
  } `additionalKeysP`
  [ ("M4-S-z", spawn "xscreensaver-command -lock")
  , ("C-<Print>", spawn "sleep 0.2; scrot -s")
  , ("<Print>", spawn "scrot")
  , ("M4-s", spawn "synapse")
  -- , ("M4-p", spawn $ "exe=`dmenu_run -b -i -p 'exec ' -nb '#dac7b3' -nf black"
  --            ++ " -sf black -sb '#e1f1f6' -fn"
  --            ++ " 'inconsolatazi4-13:antialias=true:bold'"
  --            ++ " ` && eval \"exec $exe\"")
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

myHandleEventHook :: Event -> X All
myHandleEventHook e = return (All True)

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


-- 10:55 < antho_> Can anyone tell me if it's possible to modify the clientMask?
-- 10:56 < geekosaur> not directly although you could conceivably hook something to register a
--                    different one after the window's been through X.O.windows (this probably means a
--                    layout modifier given its ordering)
-- 10:58 < antho_> I see, thanks
-- 10:58 -!- antho [~anthony@pool-71-189-186-98.lsanca.fios.verizon.net] has quit [Ping timeout: 256
--           seconds]
-- 11:01 < antho_> sorry, I'm a little confused. I can register one after the fact on a per-window
--                 basis?
-- 11:05 < geekosaur> io $ selectInput d w clientMask (see
-- http://xmonad.org/xmonad-docs/xmonad/XMonad-Operations.html#v:setInitialProperties)
-- 11:05 < geekosaur> you can't change what that one does, but you can make the same call with your
--                    preferred mask afterward
-- 11:06 < geekosaur> actually it looks like that happens early enough that you could do it in the
--                    manageHook instead, which would be easier to make it per window
-- 11:07 < geekosaur> (but test that... I notice it does the runQuery before calling windows, but it's
--                    windows that applies it so I assume laziness defers stuff until it's actually
--                    safe to do...)

--  LocalWords:  updateGlobalLogger handleEventHook geekosaur
--  LocalWords:  notatray
