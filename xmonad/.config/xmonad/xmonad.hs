import XMonad
import System.Exit
import qualified XMonad.StackSet as W

import Data.Char
import qualified Data.Map as M

import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat

import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.SetWMName
-- For Xmobar
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Hacks

import XMonad.Actions.WithAll (sinkAll)
import XMonad.Actions.CycleWS (nextScreen, prevScreen)

myTerminal      = "kitty"
myShell = "fish"
myFm = "pcmanfm"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 2
myWindowGaps    = 6

myModMask       = mod4Mask

myWorkspaces    = ["\xf829","\xf19d","\xf0239","\xebca","\xf0388"]
myNormalBorderColor  = "#1e1e2e"
myFocusedBorderColor = "#f5e0dc"
myTrayer = "killall trayer || trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x1e1e2e "

myScratchPads = [ NS "terminal" spawnTerm findTerm manageScratchpad
                , NS "btop" spawnBtop findBtop manageScratchpad
                , NS "mpd" spawnMpd findMpd manageScratchpad
                , NS "mangalnew" spawnMangalnew findMangalnew manageScratchpad
                , NS "mangal" spawnMangal findMangal manageScratchpad
                , NS "ranger" spawnRanger findRanger manageScratchpad
                , NS "newsboat" spawnNewsboat findNewsboat manageScratchpad
                ]
  where
    spawnTerm  = myTerminal ++ " --class s-terminal -e " ++ myShell
    findTerm   = appName =? "s-terminal"
    manageScratchpad = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnBtop  = myTerminal ++ " --class s-btop -e btop"
    findBtop   = appName =? "s-btop"

    spawnMpd  = myTerminal ++ " --class s-mpd -e ncmpcpp"
    findMpd   = appName =? "s-mpd"

    spawnRanger  = myTerminal ++ " --class s-ranger -e ranger"
    findRanger   = appName =? "s-ranger"

    spawnNewsboat  = myTerminal ++ " --class s-newsboat -e newsboat"
    findNewsboat   = appName =? "s-newsboat"

    spawnMangal  = myTerminal ++ " --class s-mangal -e mangal -c"
    findMangal   = appName =? "s-mangal"

    spawnMangalnew  = myTerminal ++ " --class s-mangalnew -e mangal"
    findMangalnew   = appName =? "s-mangalnew"

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True


tall     = renamed [Replace "[]="]
           $ mySpacing myWindowGaps
           $ ResizableTall 1 (3/100) (1/2) []
monacle     = renamed [Replace "[M]"]
           $ Full

floats   = renamed [Replace "><>"]
           $ noBorders
           $ simplestFloat

myLayout = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = withBorder myBorderWidth tall
                                    ||| noBorders monacle
                                    ||| floats

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Nitrogen"           --> doFloat
    , appName =? "sxiv-wall"           --> doFloat
    , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 2 )
    , title =? "LibreWolf"     --> doShift ( myWorkspaces !! 2 )
    , className =? "Emacs"     --> doShift ( myWorkspaces !! 3 )
    , className =? "pw"     --> doShift ( myWorkspaces !! 1 )
    , className =? "spotify"     --> doShift ( myWorkspaces !! 4 )
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ] <+> namedScratchpadManageHook myScratchPads



myStartupHook = do
  spawnOnce "xset r rate 300 50 &"
  -- For Extra Monitor
  -- spawnOnce "xmobar -x 1 &"
  spawnOnce "lxsession &"
  spawnOnce "picom --experimental-backends &"
  spawnOnce "$HOME/.local/bin/setbg &"
  spawnOnce "$HOME/.local/bin/tray_items  &"
  setWMName "LG3D"

myKeys =
    -- launch a terminal
    [ ("M-<Return>", spawn (myTerminal ++ " -e " ++ myShell))

    -- launch dmenu
    , ("M-p", spawn "dmenu_run")
    , ("M-b", spawn "librewolf || firefox")
    , ("M-S-w", spawn "$HOME/.local/bin/setbg")
    , ("M-<Space>", spawn "emacsclient -ca 'emacs'")
      -- ScratchPads

    , ("M-s h", namedScratchpadAction myScratchPads "btop")
    , ("M-s e", namedScratchpadAction myScratchPads "ranger")
    , ("M-s m", namedScratchpadAction myScratchPads "mpd")
    , ("M-s n", namedScratchpadAction myScratchPads "newsboat")
    , ("M-s o m", namedScratchpadAction myScratchPads "mangal")
    , ("M-s o S-m", namedScratchpadAction myScratchPads "mangalnew")
    , ("M-s t", namedScratchpadAction myScratchPads "terminal")
    -- close focused window
    , ("M-S-c", kill)

    -- Adjust Window Size unable to use -= keys :(
    , ("C-M1-j", decWindowSpacing 4)
    , ("C-M1-k",  incWindowSpacing 4)

    -- FullScreen
    , ("M-S-f",sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
    , ("M-t", sendMessage $ JumpToLayout "[]=" )
    , ("M-m", sendMessage $ JumpToLayout "[M]" )
    , ("M-f", sendMessage $ JumpToLayout "><>" )
    , ("M-C-f", spawn "full")
    , ("M-S-t", sinkAll)
     -- Rotate through the available layout algorithms
    , ("M-<Tab>", sendMessage NextLayout)


    -- Resize viewed windows to the correct size
    , ("M-n", refresh)



    -- Move focus to the next window
    , ("M-j", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-k", windows W.focusUp  )

    -- Swap the focused window and the master window
    , ("M-S-<Return>", windows W.swapMaster)

    -- Swap the focused window with the next window
    , ("M-S-j", windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ("M-S-k", windows W.swapUp    )

    -- Shrink the master area
    , ("M-h", sendMessage Shrink)

    -- Expand the master area
    , ("M-l", sendMessage Expand)

    -- Push window back into tiling
    , ("M-S-<Space>", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ("M-i", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-d", sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ("M-S-q", spawn "lxsession-logout")

    -- Restart xmonad
    , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")

    , ("M-.", nextScreen)
    , ("M-,", prevScreen)
    , ("M-;", spawn myTrayer )

    , ("<XF86AudioMute>", spawn "pamixer -t 2 && $HOME/.local/bin/notiplay")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10 || light -A 10 ")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10 || light -U 10")
    , ("<XF86AudioLowerVolume>",  spawn "pamixer -d 2 && $HOME/.local/bin/notiplay")
    , ("<XF86AudioRaiseVolume>",  spawn "pamixer -i 2 && $HOME/.local/bin/notiplay")
    , ("<Print>", spawn "flameshot gui") ]



myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((mod4Mask, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    , ((mod4Mask .|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    ]

myXmobarPP :: PP
myXmobarPP = filterOutWsPP [scratchpadWorkspaceTag] def
    { ppCurrent         = wrap " " "" . xmobarBorder "Top" peach 2
    , ppSep =  " "
    , ppHidden          = lavender . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder  = \(ws:l:t:_) -> [ws,l,t]
    }
  where
    lowWhite,  red, lavender, yellow :: String -> String
    peach :: String
    peach  =  "#fab387"
    lavender    = xmobarColor "#b4befe" ""
    yellow   = xmobarColor "#f9e2af" ""
    red      = xmobarColor "#f38ba8" ""
    lowWhite = xmobarColor "#45475a" ""

main :: IO ()
main = xmonad
     -- . ewmhFullscreen -- Uncomment to disable Pseudo FullScreen
     . ewmh
     . docks
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (clickablePP myXmobarPP)) toggleStrutsKey
     $ myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m .|. shiftMask, xK_b)
myConfig = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        mouseBindings      = myMouseBindings,
        layoutHook         = smartBorders myLayout,
        manageHook         = myManageHook,
        handleEventHook    = windowedFullscreenFixEventHook <> swallowEventHook (className =? "Alacritty" <||> className =? "st-256color") (return True),
        startupHook        = myStartupHook
    }

    `additionalKeysP` myKeys
