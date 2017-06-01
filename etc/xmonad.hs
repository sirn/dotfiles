import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig(additionalKeys)

baseConfig = xfceConfig

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

fullscreenSupport :: XConfig a -> XConfig a
fullscreenSupport c = c
  { startupHook = startupHook c +++ setSupportedWithFullscreen }
  where x +++ y = mappend x y

myManageHooks :: ManageHook
myManageHooks = composeAll
  [ className =? "Onboard"         --> doIgnore
  , className =? "Xfce4-appfinder" --> doFloat
  , className =? "Xfrun4"          --> doFloat ]

myModMask :: KeyMask
myModMask = mod4Mask

myAdditionalKeys :: [((ButtonMask, KeySym), X ())]
myAdditionalKeys =
  [ ((0, 0x1008FF11),   spawn "pamixer -d 10")
  , ((0, 0x1008FF13),   spawn "pamixer -i 10")
  , ((myModMask, xK_p), spawn "dmenu_run -fn \"Source Code Pro-10\" -lh 40")
  ]

main :: IO ()
main = do
  xmonad $ fullscreenSupport $ ewmh baseConfig
    { terminal        = "urxvt"
    , modMask         = myModMask
    , handleEventHook = handleEventHook baseConfig <+> fullscreenEventHook
    , manageHook      = manageHook baseConfig <+> myManageHooks
    } `additionalKeys` myAdditionalKeys
