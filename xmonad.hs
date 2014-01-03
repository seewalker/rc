import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Config.Xfce
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

myModMask = mod1Mask

--My urxvt modMask is also the alt key, so do not override the sequences...
--  M-c ::= urxvt copy
--  M-v ::= urxvt paste
--  M-u ::= urxvt url-mode
main = xmonad $ xfceConfig
    {  terminal = "urxvt"
    ,  XMonad.focusFollowsMouse = True
    ,  modMask = myModMask
    ,  borderWidth = 2
    ,  normalBorderColor = "#cccccc"
    ,  focusedBorderColor = "#8fd8d8"
    ,  manageHook = manageDocks <+> manageHook defaultConfig
    ,  layoutHook = avoidStruts $ layoutHook defaultConfig
    } `additionalKeys`
    [ ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -activate")
    ]
