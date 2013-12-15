import XMonad
import XMonad.Config.Xfce

main = xmonad xfceConfig
    {  terminal = "urxvt"
    ,  borderWidth = 2
    ,  normalBorderColor = "#cccccc"
    ,  focusedBorderColor = "#cd8b00"}
