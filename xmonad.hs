 import XMonad
 import XMonad.Layout.NoBorders
 import XMonad.Layout.PerWorkspace
 import XMonad.Layout.Spacing
 import XMonad.Layout.Grid
 import XMonad.Layout.IM
 import XMonad.Layout.Reflect
 import XMonad.Hooks.FadeInactive
 import Graphics.X11.ExtraTypes.XF86
 import XMonad.Hooks.DynamicLog
 import XMonad.Hooks.ManageDocks
 import XMonad.Util.Run
 import XMonad.Util.EZConfig
 import XMonad.Util.Scratchpad
 import Control.Monad
 import qualified XMonad.StackSet as W
 import qualified Data.Map as M
 import System.IO

 myLayout =
         avoidStrutsOn [U] $ onWorkspace "4:IM" myIMLayout $
          tiled ||| Mirror tiled ||| Full where
         tiled = spacing 5 $ Tall nmaster delta ratio
         reflectTiled = (reflectHoriz tiled)
         nmaster = 1
         ratio = 2/3
         delta = 5/100
         nobordersLayout = smartBorders $ Full
         gridLayout = spacing 8 $ Grid ||| Mirror tiled
         myIMLayout = avoidStruts $ smartBorders $ withIM ratio skypeRoster $ reflectHoriz $
                                                                  withIM ratio jitsiRoster chatLayout where
                 chatLayout = Grid ||| Full
                 ratio = 1/8
                 jitsiRoster = (ClassName "Jitsi") `And` (Title "Jitsi")
                 skypeRoster = (ClassName "Skype") `And`
                                (Not (Title "Options")) `And`
                                               (Not (Role "ConversationsWindow")) `And`
                                                              (Not (Role "CallWindow"))

 myWorkspaces = ["1:timeout","2:lab", "3:engineering","4:IM", "5:mail"]

 myTerminal = "urxvt"

 myManageHook = composeAll
              [ className =? "Firefox"                                   --> viewShift "2"
              , (className =? "Rekonq" <&&> resource =? "Dialog")        --> doFloat
              , className =? "yakuake"                                   --> doFloat
              , className =? "Yakuake"                                   --> doFloat
              , title =? "mail"                                          --> doShift "5:mail"
              , title =? "irc"                                           --> doShift "4:IM"
              , className =? "plasma"                                    --> doFloat
              , className =? "Plasma"                                    --> doFloat
              , className =? "Plasma-desktop"                            --> doFloat
              , className =? "plasma-desktop"                            --> doFloat
              , className =? "plasmashell"                               --> doFloat
              , className =? "ksplashsimple"                             --> doFloat
              , className =? "krunner"                                   --> doFloat
              , className =? "ksplashqml"                                --> doFloat
              , className =? "ksplashx"                                  --> doFloat
              , manageDocks
              ]
              where
                    viewShift    = doF . liftM2 (.) W.greedyView W.shift
                    classNotRole :: (String, String) -> Query Bool
                    classNotRole (c,r) = className =? c <&&> role =? r
                    role = stringProperty "WM_WINDOW_ROLE"

 myLogHook :: X ()
 myLogHook = fadeInactiveLogHook fadeAmount
           where
             fadeAmount = 0.6
             hideScratchpad ws = if ws == "NSP" then "" else pad ws -- don't show scratchpad in workspace list

 manageScratchPad :: ManageHook
 manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
   where
     h = 0.4
     w = 0.8
     t = 0
     l = 0.1

 main = do
     xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
     xmonad $ defaultConfig { modMask = mod4Mask
                            , layoutHook = avoidStruts $ myLayout
                            , terminal = "urxvt"
                            , handleEventHook = docksEventHook
                            , workspaces = myWorkspaces
                            , manageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks <+> manageScratchPad
                            , logHook = myLogHook <+> dynamicLogWithPP xmobarPP
                              -- { ppOutput = hPutStrLn xmproc
                              -- , ppTitle = xmobarColor "blue" "" . shorten 50
                              -- , ppLayout = const ""
                              -- }
                            }
                            `additionalKeys`
                            [(( mod4Mask .|. shiftMask, xK_e ), return ())
                            ,(( mod4Mask .|. shiftMask, xK_r ), return ())
                            ,(( mod4Mask, xK_h ), return ())
                            ,(( mod4Mask, xK_l ), return ())
                            ,(( mod4Mask, xK_Escape ), spawn "xmonad --recompile; xmonad --restart")
                            ,(( mod4Mask, xK_minus ), sendMessage Shrink)
                            ,(( mod4Mask, xK_equal ), sendMessage Expand)
                            ,(( mod4Mask .|. shiftMask, xK_e ), spawn "emacsclient -c")
                            ,(( mod4Mask .|. shiftMask, xK_m ), spawn "chromium --force-device-scale-factor=2")
                            ,(( mod4Mask .|. shiftMask, xK_f ), spawn "firefox")
                            ,(( mod4Mask .|. shiftMask, xK_r ), spawn "conkeror")
                            ,(( mod4Mask .|. shiftMask, xK_l ), spawn "conkeror https://slack.com/signin")
                            ,(( mod4Mask .|. shiftMask, xK_u ), spawn "LC_CTYPE=ja_JP.UTF-8 emacs --title 'mail' -f mu4e ")
                            ,(( mod4Mask .|. shiftMask, xK_i ), spawn "emacs --title 'irc' -f irc-connect -f make-frame-command -f")
                            ,(( mod4Mask .|. shiftMask, xK_o ), scratchPad)
                            ,((0                      , 0xff57), scratchPad)
                            ,((0                      , xF86XK_MonBrightnessDown), spawn "xbacklight - 10")
                            ,((0                      , xF86XK_MonBrightnessUp), spawn "xbacklight + 10")
                            ,((0                      , xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
                            ,((0                      , xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
                            ,((0                      , xF86XK_AudioMute), spawn "amixer set Master toggle; amixer set Speaker toggle")
                            ]

     where
       scratchPad = scratchpadSpawnActionTerminal myTerminal
