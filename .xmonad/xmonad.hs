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
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Util.Run ( hPutStrLn, spawnPipe )
import XMonad.Util.EZConfig ( additionalKeysP, additionalMouseBindings )


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
myPrimaryColor = "#f88017"
myAccentColor = "#333333"
myLayout = avoidStruts $ smartBorders $
  ResizableTall 1 0.03 0.618 [] |||
  Full |||
  Grid


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
        , ppTitle = xmobarColor myPrimaryColor "" . shorten 50
        }
        >> updatePointer (Relative 0.5 0.5)

    , modMask = myModMask
    , terminal = myTerminal

    , borderWidth = 1
    , normalBorderColor = myAccentColor
    , focusedBorderColor = myPrimaryColor

    , focusFollowsMouse = False
    }

    `additionalKeysP`
    [
    -- grid select with `super + a`
      ("M-a", goToSelected defaultGSConfig)

    -- quick `full` layout with `super + f11`
    , ("M-<F11>", sendMessage $ JumpToLayout "Full")

    -- `ResizableTall` sizing with
    --   `super + shift + h` and `super + shift + l`
    , ("M-S-h", sendMessage MirrorExpand)
    , ("M-S-l", sendMessage MirrorShrink)

    -- volume control, via `pactl`
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +1%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -1%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle" )
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
