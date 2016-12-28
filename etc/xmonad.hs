import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)

baseConfig = desktopConfig

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ fullscreenSupport $ ewmh baseConfig
    { terminal        = "urxvt"
    , modMask         = mod4Mask
    , logHook         = dynamicLogString basePP >>= xmonadPropLog
    , handleEventHook = handleEventHook baseConfig <+> fullscreenEventHook
    } `additionalKeys` myAdditionalKeys

myAdditionalKeys :: [((ButtonMask, KeySym), X ())]
myAdditionalKeys =
  [ ((0, 0x1008FF11), spawn "pamixer -d 10")
  , ((0, 0x1008FF13), spawn "pamixer -i 10")
  ]

fullscreenSupport :: XConfig a -> XConfig a
fullscreenSupport c = c
  { startupHook = startupHook c +++ setSupportedWithFullscreen }
  where x +++ y = mappend x y

setSupportedWithFullscreen :: X ()
setSupportedWithFullscreen = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_FULLSCREEN"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)
    setWMName "xmonad"

basePP :: PP
basePP =
  xmobarPP { ppOrder   = \(ws:l:_:_) -> [ws,l]
           , ppCurrent = xmobarColor "#000" "#fff" . pad
           , ppHidden  = xmobarColor "#fff" "#666" . pad
           , ppLayout  = xmobarColor "gray" ""
           , ppSep     = " "
           , ppWsSep   = ""
           }
