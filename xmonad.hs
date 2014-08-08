import XMonad           
import XMonad.Layout.NoBorders  
import XMonad.Layout.PerWorkspace  
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import Control.Monad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.IO

myLayout = 
        avoidStrutsOn [U] $ onWorkspace "3:IM" myIMLayout $
         tiled ||| Mirror tiled ||| Full where
        tiled = spacing 5 $ Tall nmaster delta ratio
        reflectTiled = (reflectHoriz tiled)
        nmaster = 1
        ratio = 2/3
        delta = 5/100
        nobordersLayout = smartBorders $ Full
        gridLayout = spacing 8 $ Grid
        myIMLayout = avoidStruts $ smartBorders $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| reflectTiled ||| Grid) where
                chatLayout = Grid
                ratio = 1/9
                skypeRatio = 1/8
                emacsRoster = And (ClassName "Emacs") (Title "irc")
                skypeRoster = (ClassName "Skype") `And`
                               (Not (Title "Options")) `And`
                                              (Not (Role "ConversationsWindow")) `And`
                                                             (Not (Role "CallWindow"))


myWorkspaces = ["1:coding","2","3:IM", "4:mail"]

myManageHook = composeAll
             [ className =? "Firefox"                                   --> viewShift "2"
             , (className =? "Rekonq" <&&> resource =? "Dialog")        --> doFloat
             , className =? "Yakuake"                                   --> doFloat
             , title =? "mail"                                          --> doShift "4:mail"
             , title =? "irc"                                           --> doShift "3:IM"
             , className =? "Skype"                                     --> doShift "3:IM"
             , classNotRole ("Skype", "MainWindow")                     --> doFloat
             , manageDocks
             ]
             where 
                   viewShift    = doF . liftM2 (.) W.greedyView W.shift       
                   classNotRole :: (String, String) -> Query Bool
                   classNotRole (c,r) = className =? c <&&> role =? r
                   role = stringProperty "WM_WINDOW_ROLE"

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
          where fadeAmount = 0.6

main = do  
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc" 
    xmonad $ defaultConfig { modMask = mod4Mask
                           , layoutHook = avoidStruts $ myLayout
                           , terminal = "urxvt"
                           , handleEventHook = docksEventHook
                           , workspaces = myWorkspaces
                           , manageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks
                           , logHook = myLogHook <+> dynamicLogWithPP xmobarPP
                             { ppOutput = hPutStrLn xmproc
                             , ppTitle = xmobarColor "blue" "" . shorten 50
                             , ppLayout = const ""
                             }
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
                           ,(( mod4Mask .|. shiftMask, xK_r ), spawn "conkeror")
                           ,(( mod4Mask .|. shiftMask, xK_l ), spawn "conkeror https://slack.com/signin")
                           ,(( mod4Mask .|. shiftMask, xK_u ), spawn "emacs --title 'mail' -f mu4e ")
                           ,(( mod4Mask .|. shiftMask, xK_i ), spawn "emacs --title 'irc' -f irc-connect")
                           ,(( mod4Mask .|. shiftMask, xK_s ), spawn "skype")
                           ,(( mod4Mask .|. shiftMask, xK_f ), spawn "firefox")
                           ]



