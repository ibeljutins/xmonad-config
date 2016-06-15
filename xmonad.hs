import Data.Maybe(maybe)
import qualified Data.Map as M
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

import XMonad.Actions.GridSelect
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer

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
myKeys = [ ((myModMask, xK_z), sendMessage MirrorShrink)               -- Vertical
         , ((myModMask .|. shiftMask, xK_z), sendMessage MirrorExpand) -- resizing
         , ((myModMask, xK_w), viewScreen 0)
         , ((myModMask, xK_r), viewScreen 1)
         , ((myModMask, xK_x), viewScreen 2)
         , ((myModMask, xK_o), scratchPad)
         , ((myModMask, xK_u), spawn "xrandr-invert-colors -s 0")
         --, ("S-<Print>", spawn "/usr/bin/xcalib -i -a")
         --, ((shiftMask, xK_Print), spawn "/usr/bin/xcalib -i -a")
         , ((0, xK_Print), spawn "xrandr-invert-colors")
         , ((myModMask, xK_a), submap . M.fromList $
            [ ((0, xK_n),     spawn "amarok -f")
            , ((0, xK_p),     spawn "amarok -r")
            , ((0, xK_space), spawn "amarok -t")
            ])
         , ((myModMask, xK_F8), spawn "amixer set Master 6-")
         , ((myModMask, xK_F9), spawn "amixer set Master 6+")
         , ((myModMask, xK_g), goToSelected defaultGSConfig)
         , ((myModMask, xK_s), spawnSelected defaultGSConfig [ "/opt/qtcreator/bin/qtcreator"
                                                             , "systemsettings5"
                                                             , "/usr/bin/vivaldi"
                                                             , "dolphin"
                                                             , "kontact"
                                                             , "davmail"
                                                             , "plasmashell"
                                                             , "krdc"
                                                             , "amarok"
                                                             ])
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
  , [ className =? c --> doF (W.shift "3") | c <- browsers]
  , [ className =? c --> doF (W.shift "4") | c <- docreaders]
  , [ className =? c --> doF (W.shift "5") | c <- dolphin]
  , [ className =? c --> doF (W.shift "8") | c <- messengers]
  , [ className =? c --> doF (W.shift "9") | c <- audioplayers]
  ]
  where
    myFloats = [ "MPlayer"
               , "Gimp"
               , "Plasma-desktop"
               , "plasma-desktop"
               , "Plasmashell"
               , "plasmashell"
               , "Krunner"
               , "krunner"
               , "KCalc"
               , "Klipper"
               , "Keepassx"
               , "systemsettings"
               , "Systemsettings5"
               , "Yakuake"
               , "Khelpcenter"
               , "khelpcenter"
               , "kde5-nm-connection-editor"
               , "Cantata"
               ]
    mailApps     = ["Kmail", "Kontact"]
    browsers     = ["Firefox", "Vivaldi-bin"]
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
desktop "/usr/share/xsessions/plasma" = kde4Config
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
    then do
      runCommand "/bin/bash ~/.xmonad/xprofile"
      mproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
      xmonad $ myDesktopConfig { logHook = myLogHook mproc }
    else do
      xmonad myDesktopConfig { logHook = updatePointer (0.5, 0.5) (0, 0) }
