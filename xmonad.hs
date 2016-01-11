import Data.Maybe(maybe)
import System.Directory
import System.IO
import System.Posix.Env(getEnv)
import System.Process

import XMonad

import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce

import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed

import XMonad.Actions.PhysicalScreens

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad

import qualified XMonad.StackSet as W

-- basic configuration
myModMask     = mod4Mask  -- use the Windows key as mod
myBorderWidth = 1         -- set window border size
myTerminal    = "konsole" -- preferred terminal emulator

-- key bindings
myKeys = [ ((myModMask, xK_a), sendMessage MirrorShrink)
         , ((myModMask, xK_z), sendMessage MirrorExpand)
         , ((myModMask, xK_w), viewScreen 0)
         , ((myModMask, xK_e), viewScreen 1)
         , ((myModMask, xK_o), scratchPad)
         , ((myModMask, xK_u), spawn "/usr/bin/xcalib -i -a")
         , ((0, xK_Print), spawn "/usr/bin/xcalib -i -a")
         ]
 where
   scratchPad = scratchpadSpawnActionTerminal myTerminal

-- hooks for newly created windows
-- note: run 'xprop WM_CLASS' to get className
myManageHook :: ManageHook
myManageHook = manageDocks <+> manageScratchPad <+> coreManageHook

coreManageHook :: ManageHook
coreManageHook = composeAll . concat $
  [ [ className =? c --> doFloat           | c <- myFloats]
  , [ className =? c --> doF (W.shift "2") | c <- browsers]
  , [ className =? c --> doF (W.shift "4") | c <- docreaders]
  , [ className =? c --> doF (W.shift "5") | c <- dolphin]
  , [ className =? c --> doF (W.shift "8") | c <- messengers]
  , [ className =? c --> doF (W.shift "9") | c <- audioplayers]
  ]
  where
    myFloats = [ "MPlayer"
               , "Gimp"
               , "Plasma-desktop"
               , "plasmashell"
               , "KCalc"
               , "Klipper"
               , "Keepassx"
               , "systemsettings"
               , "Systemsettings5"
               , "Yakuake"
               , "kde5-nm-connection-editor"
               ]
    mailApps     = ["Kmail"]
    browsers     = ["Firefox"]
    dolphin      = ["Dolphin"]
    docreaders   = ["Okular"]
    messengers   = ["Skype"]
    audioplayers = ["Amarok"]

-- yakuake style hook
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.4     -- terminal height, 40%
    w = 1       -- terminal width, 100%
    t = 1 - h   -- distance from top edge, 90%
    l = 1 - w   -- distance from left edge, 0%

-- startup hooks
myStartupHook = setWMName "LG3D"

-- layout hooks
myLayoutHook = smartBorders $ avoidStruts $ coreLayoutHook

coreLayoutHook = tiled ||| Mirror tiled ||| Full ||| Grid
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = ResizableTall nmaster delta ratio []
    nmaster = 1     -- The default number of windows in the master pane
    ratio   = 1/2   -- Default proportion of screen occupied by master pane
    delta   = 3/100 -- Percent of screen to increment by when resizing panes


-- log hook (for xmobar)
myLogHook xmproc = dynamicLogWithPP xmobarPP
  { ppOutput = hPutStrLn xmproc
  , ppTitle  = xmobarColor "green" "" . shorten 50
  }

-- desktop :: DESKTOP_SESSION -> desktop_configuration
desktop "gnome"        = gnomeConfig
desktop "xmonad-gnome" = gnomeConfig
desktop "kde"          = kde4Config
desktop "kde-plasma"   = kde4Config
desktop "plasma"       = kde4Config
desktop "xfce"         = xfceConfig
desktop _              = desktopConfig

--
-- main function (no configuration stored there)
--

main :: IO ()
main = do
  session <- getEnv "DESKTOP_SESSION"
  let defDesktopConfig = maybe desktopConfig desktop session
      myDesktopConfig = defDesktopConfig { modMask     = myModMask
                                         , borderWidth = myBorderWidth
                                         , startupHook = myStartupHook
                                         , layoutHook  = myLayoutHook
                                         , manageHook  = myManageHook <+> manageHook defDesktopConfig
                                         } `additionalKeys` myKeys
  xmobarInstalled <- doesFileExist "/usr/bin/xmobar"
  if session == Just "xmonad" && xmobarInstalled
    then do runCommand "/bin/bash ~/.xmonad/xprofile"
            mproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
            xmonad $ myDesktopConfig { logHook = myLogHook mproc }
    else do xmonad myDesktopConfig
