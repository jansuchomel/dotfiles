import           XMonad

import           XMonad.Util.EZConfig

import           XMonad.Actions.PhysicalScreens
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.NoBorders
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Shell
import           XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet                    as S

import           XMonad.Config.Desktop

import           System.Exit                        (exitSuccess)

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

-- WS index name key clickable
data WS = WS Integer String KeySym String

instance Show WS where
    show ( WS _ _ _ clickable ) = clickable

makeClickable :: Integer -> String -> String
makeClickable index name = "<action=wmctrl -s " ++ show index ++ ">" ++ name ++ "</action>"

makeWS :: Integer -> String -> KeySym -> WS
makeWS index name key = WS index name key ( makeClickable index name )

tripleToWS :: (Integer, String, KeySym) -> WS
tripleToWS (index, name, key) = makeWS index name key

myWorkSpaceNames :: [String]
myWorkSpaceNames = ["sys", "web", "read", "fs", "play", "code", "seven", "eight", "nine",
                    "ten", "-", "="]

myWorkSpaceKeys :: [KeySym]
myWorkSpaceKeys = [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal]

myWorkSpaces :: [WS]
myWorkSpaces = map tripleToWS $ zip3 [0..] myWorkSpaceNames myWorkSpaceKeys

myNormalBorderColor :: String
myNormalBorderColor  = "#212121"
myFocusedBorderColor :: String
myFocusedBorderColor = "#C2185B"

myBSP = (smartBorders . smartSpacing 3 ) emptyBSP
myTabbed = noBorders ( tabbedBottom shrinkText defaultTheme {fontName="xft:Fira Mono-11"} )
myLayoutHook = myBSP ||| myTabbed ||| simpleFloat

myManageHook :: ManageHook
myManageHook = composeAll
      [
        manageDocks,
        className =? "mpv" --> doFloat,
        className =? "Pavucontrol" --> doFloat,
        (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat,
        (className =? "Firefox-developer-edition" <&&> resource =? "Dialog") --> doFloat
      ]


myPP :: PP
myPP =  defaultPP
  {
    ppHiddenNoWindows = const ""
  , ppHidden          = xmobarColor "#727272" ""
  , ppTitle           = xmobarColor "#ffffff"  ""
  , ppCurrent         = xmobarColor "#C2185B" ""
  , ppSort            = getSortByIndex
  , ppLayout          = layoutFormat
  , ppSep             = " "
  }


myXPC :: XPConfig
myXPC = defaultXPConfig
  {
      font = "xft:Fira Mono-11"
    , historySize = 100
    , position = Top
    , fgColor = "#ffffff"
    , bgColor = "#1d262a"
    , promptBorderWidth = 0
  }


myBar :: String
myBar = "xmobar"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

workSpaceSwitch :: WS -> ((KeyMask, KeySym), X ())
workSpaceSwitch (WS _ _ key clickable) =
    ( (myModMask, key), windows $ S.greedyView clickable )
workSpaceMove :: WS -> ((KeyMask, KeySym), X ())
workSpaceMove (WS _ _ key clickable) =
    ( (myModMask .|. shiftMask, key), windows $ S.shift clickable )

defaults = desktopConfig {
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        workspaces         = map show myWorkSpaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        layoutHook = lessBorders OnlyFloat $ avoidStruts myLayoutHook,
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
           -- , ((myModMask,                 xK_b         ), sendMessage ToggleStruts)
           , ((0,                         0x1008FF02   ), spawn "exe=`xbacklight -inc 20`")
           , ((0,                         0x1008FF03   ), spawn "exe=`xbacklight -dec 20`")
           , ((0,                         0x1008FF11   ), spawn "exe=`pulsemixer --id 1 --change-volume -5`")
           , ((0,                         0x1008FF13   ), spawn "exe=`pulsemixer --id 1 --change-volume +5`")
           , ((myModMask,                 xK_d         ), shellPrompt myXPC)
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
           , ((myModMask .|. shiftMask,   xK_q         ), confirmPrompt myXPC "exit" $ io exitSuccess)
           ]
           ++
           map workSpaceSwitch myWorkSpaces
           ++
           map workSpaceMove myWorkSpaces
    )



main :: IO ()
main = xmonad =<< (statusBar myBar myPP toggleStrutsKey . ewmh) defaults
