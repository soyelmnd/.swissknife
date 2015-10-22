-- dependencies
--   xmonad
--   xmonad-contrib
--   xmobar
--   trayer
-- @see
--   http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Config.html
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad hiding ( (|||) )
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers ( isFullscreen, isDialog, doFullFloat, doCenterFloat )
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run ( hPutStrLn, spawnPipe )
import XMonad.Util.EZConfig ( additionalKeys, additionalMouseBindings )


myModMask = mod4Mask
myTerminal = "xfce4-terminal"
myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- application, for clustering
myAppChat = [ "Pidgin", "Skype", "Slack" ]
myAppVirtualMachine = [ "VirtualBox" ]
myAppGraphic = [ "Gimp" ]
myAppEntertainment = []
myAppIgnored = [ "trayer" ]

-- theme and layout config
myActiveColor = "#f88017"
myLayout = avoidStruts $ smartBorders $
  Tall 1 0.03 0.618 |||
  Full |||
  ThreeColMid 1 0.03 0.5


main = do
  -- spawn a handy xmobar with a trayer
  xmproc <- spawnPipe "$HOME/.xmonad/trayer-factory"
  xmproc <- spawnPipe "/usr/bin/xmobar $HOME/.xmonad/xmobar.hs"

  xmonad $ defaultConfig
    {
      workspaces = myWorkspaces
    , manageHook = manageDocks <+> workspaceCluster <+> manageHook defaultConfig
    , layoutHook = myLayout
    , logHook = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor myActiveColor "" . shorten 50
        }
        >> updatePointer (Relative 0.5 0.5)

    , modMask = myModMask
    , terminal = myTerminal

    , borderWidth = 1
    , normalBorderColor = "#000000"
    , focusedBorderColor = myActiveColor

    , focusFollowsMouse = False
    }

    `additionalKeys`
    [
    -- grid select with `super + a`
      ((mod4Mask, xK_a), goToSelected defaultGSConfig)

    -- quick `full` layout with `super + f11`
    , ((mod4Mask, xK_F11), sendMessage $ JumpToLayout "Full")

    -- volume control, via `pactl`
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +1%")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -1%")
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle" )
    ]

    `additionalMouseBindings`
    [
      -- @todo implement `click to scrot`
    ]

  where
    -- workspaces and workspace cluster
    workspaceCluster  = composeAll . concat $
      [
        [ className =? c --> doShift "9" | c <- myAppChat ]
      , [ className =? c --> doShift "8" | c <- myAppVirtualMachine ]
      , [ className =? c --> doShift "7" | c <- myAppGraphic ]
      , [ className =? c --> doShift "6" | c <- myAppEntertainment ]
      , [ resource =? r --> doIgnore | r <- myAppIgnored ]
      , [ isFullscreen --> doFullFloat ]
      , [ isDialog --> doCenterFloat ]
      ]
