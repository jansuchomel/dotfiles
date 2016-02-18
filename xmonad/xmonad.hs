import           XMonad

import           XMonad.Util.EZConfig

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.WorkspaceCompare
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Tabbed
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Spacing
import           XMonad.Layout.SimpleFloat
import           XMonad.Actions.PhysicalScreens

import qualified XMonad.StackSet as S

import           XMonad.Config.Desktop

clickable :: Show a => (a, String, t, t1) -> String
clickable (index, name, _, _) = "<action=wmctrl -s " ++ show index ++ ">" ++ name ++ "</action>"

layoutFormat :: String -> String
layoutFormat "SmartSpacing 3 BSP" = "BSP"
layoutFormat "Tabbed Bottom Simplest" = "tabbed"
layoutFormat str = str

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth   = 2

myTerminal :: String
myTerminal = "termite"

myWorkspacesCodes :: [(Integer, String, String, KeySym)]
myWorkspacesCodes = [(0, "\xf120", "1", 0x0031), (1, "\xf269", "2", 0x0032), (2, "\xf02d", "3", 0x0033), (3, "\xf07c", "4", 0x0034),
                     (4, "\xf16a", "5", 0x0035), (5, "\xf121", "6", 0x0036), (6, "\xf27b", "7", 0x0037),
                     (7, "8", "8", 0x0038), (8, "9", "9", 0x0039), (9, "0", "0", 0x0030), (10, "-", "minus", 0x002d), (11, "=", "equal", 0x003d)]

myWorkspaces :: [String]
myWorkspaces = map clickable myWorkspacesCodes


myNormalBorderColor :: String
myNormalBorderColor  = "#212121"
myFocusedBorderColor :: String
myFocusedBorderColor = "#C2185B"

myBSP = (smartBorders . ( smartSpacing 3 )) emptyBSP
myTabbed = noBorders ( tabbedBottom shrinkText defaultTheme {fontName="xft:Source Code Pro-9"} )
myLayoutHook = myBSP ||| myTabbed ||| simpleFloat

myManageHook = composeAll
      [
        manageDocks,
        resource =? "rofi" --> doFloat,
        className =? "mpv" --> doFloat,
        className =? "Pavucontrol" --> doFloat,
        (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
      ]


myPP :: PP
myPP =  defaultPP
  {
    ppHiddenNoWindows = const ""
  , ppHidden          = xmobarColor "#727272" "" . wrap "" " "
  , ppTitle           = xmobarColor "#ffffff"  "" . wrap " " ""
  , ppCurrent         = xmobarColor "#C2185B" "" . wrap "" " "
  , ppSort            = getSortByIndex
  , ppLayout          = layoutFormat
  , ppSep             = " "
  }


myBar :: String
myBar = "xmobar"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

workspaceSwitch :: Show a => (a, String, t, t1) -> ((KeyMask, t1), X ())
workspaceSwitch (index, name, key, code) = ( (myModMask, code), windows $ S.greedyView ( clickable (index, name, key, code)) )
workspaceMove :: Show a => (a, String, t, t1) -> ((KeyMask, t1), X ())
workspaceMove   (index, name, key, code) = ( (myModMask .|. shiftMask, code), windows $ S.shift ( clickable (index, name, key, code)) )

defaults = desktopConfig {
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        layoutHook = lessBorders OnlyFloat $ avoidStruts $ myLayoutHook,
        handleEventHook    = fullscreenEventHook,
        manageHook = myManageHook,
        modMask = mod4Mask

    }  `additionalKeys` -- hex codes are in /usr/include/X11/XF86keysym.h, standard keys are in /usr/include/X11/keysymdef.h
    (
           [
             ((myModMask .|. shiftMask,   xK_l         ), sendMessage $ ExpandTowards R)
           , ((myModMask .|. shiftMask,   xK_h         ), sendMessage $ ExpandTowards L)
           , ((myModMask .|. shiftMask,   xK_j         ), sendMessage $ ExpandTowards D)
           , ((myModMask .|. shiftMask,   xK_k         ), sendMessage $ ExpandTowards U)
           , ((0,                         0x1008FF02   ), spawn "exe=`xbacklight -inc 20`")
           , ((0,                         0x1008FF03   ), spawn "exe=`xbacklight -dec 20`")
           , ((0,                         0x1008FF11   ), spawn "exe=`pulsemixer --id 1 --change-volume -5`")
           , ((0,                         0x1008FF13   ), spawn "exe=`pulsemixer --id 1 --change-volume +5`")
           , ((myModMask,                 xK_d         ), spawn "rofi -show run -terminal termite")
           , ((myModMask,                 xK_p         ), spawn "rofi -show window")
           , ((myModMask,                 xK_Down      ), spawn "mpc toggle")
           , ((myModMask,                 xK_Left      ), spawn "mpc prev")
           , ((myModMask,                 xK_Right     ), spawn "mpc next")
           , ((myModMask,                 xK_r         ), sendMessage Rotate)
           , ((myModMask,                 xK_s         ), sendMessage Swap)
           , ((myModMask,                 xK_l         ), spawn "dm-tool lock")
           , ((myModMask,                 xK_BackSpace ), onPrevNeighbour S.view)
           , ((myModMask,                 xK_Tab       ), onNextNeighbour S.view)
           , ((myModMask .|. shiftMask,   xK_BackSpace ), onPrevNeighbour S.shift)
           , ((myModMask .|. shiftMask,   xK_Tab       ), onNextNeighbour S.shift)
           , ((myModMask,                 xK_m         ), spawn "clerk")
           , ((myModMask,                 xK_x         ), spawn "rofi-pass")
           ]
           ++
           map workspaceSwitch myWorkspacesCodes
           ++
           map workspaceMove myWorkspacesCodes
    )



main :: IO ()
main = do
 xmonad =<< ((statusBar myBar myPP toggleStrutsKey) . ewmh) defaults -- haskell function composition?
