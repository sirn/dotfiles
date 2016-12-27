import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)

baseConfig = desktopConfig

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ baseConfig
    { terminal   = "urxvt"
    , modMask    = mod4Mask
    , logHook    = dynamicLogString basePP >>= xmonadPropLog
    } `additionalKeys` myAdditionalKeys

myAdditionalKeys :: [((ButtonMask, KeySym), X ())]
myAdditionalKeys =
  [ ((0, 0x1008FF11), spawn "pamixer -d 10")
  , ((0, 0x1008FF13), spawn "pamixer -i 10")
  ]

basePP :: PP
basePP =
  xmobarPP { ppOrder   = \(ws:l:_:_) -> [ws,l]
           , ppCurrent = xmobarColor "#000" "#fff" . pad
           , ppHidden  = xmobarColor "#fff" "#666" . pad
           , ppLayout  = xmobarColor "gray" ""
           , ppSep     = " "
           , ppWsSep   = ""
           }
