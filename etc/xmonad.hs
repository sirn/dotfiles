import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog

baseConfig = desktopConfig

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ baseConfig
    { terminal   = "urxvt"
    , modMask    = mod4Mask
    , logHook    = dynamicLogString basePP >>= xmonadPropLog
    }

basePP :: PP
basePP =
  xmobarPP { ppOrder   = \(ws:l:_:_) -> [ws,l]
           , ppCurrent = xmobarColor "#000" "#fff" . pad
           , ppHidden  = xmobarColor "#fff" "#666" . pad
           , ppLayout  = xmobarColor "gray" ""
           , ppSep     = " "
           , ppWsSep   = ""
           }
