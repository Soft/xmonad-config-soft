module XMonad.Config.Soft.Bindings
  (keyMap) where

import qualified Data.Map as M

import XMonad (XConfig(..), (.|.), X, Query, sendMessage)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace, tagToEmptyWorkspace)
import XMonad.Actions.WindowGo (raiseMaybe)
import XMonad.Layout.Groups.Helpers (swapMaster)
import XMonad.Layout.BinarySpacePartition (ResizeDirectional(..), Direction2D(..), Rotate(..), Swap(..))
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (safeSpawn)
import XMonad.ManageHook ((=?), className, title)
import XMonad.Operations (withFocused)

import Graphics.X11.Types

import XMonad.Config.Soft.Actions
import XMonad.Config.Soft.Prompt


keyMap :: XConfig a -> M.Map (KeyMask, KeySym) (X ())
keyMap xc = mkKeymap xc
  [ ("M-0", viewEmptyWorkspace)
  , ("M-S-0", tagToEmptyWorkspace)
  , ("M-<Backspace>", toggleWS)
  , ("M-S-m", swapMaster)
  , ("M-x", rofi internalPrompt)
  , ("M-c", withFocused centerWindow)

  -- Keybindings for BinarySpacePartition
  , ("C-M-l", sendMessage $ MoveSplit R)
  , ("C-M-h", sendMessage $ MoveSplit L)
  , ("C-M-j", sendMessage $ MoveSplit D)
  , ("C-M-k", sendMessage $ MoveSplit U)
  , ("M-r", sendMessage Rotate)
  , ("M-s", sendMessage Swap) ]
  `M.union` launchersToMap xc launchers

launchersToMap :: XConfig a -> [(KeySym, String, [String], Query Bool)] -> M.Map (KeyMask, KeySym) (X ())
launchersToMap c = M.fromList . map create
  where
    create (k, b, a, p) = ((mod .|. shiftMask, k), raiseMaybe (safeSpawn b a) p)
    mod = modMask c

-- Maybe I'll make these more configurable one day
launchers :: [(KeySym, String, [String], Query Bool)]
launchers =
  [ (xK_b, "firefox", [],                     className =? "Firefox")
  , (xK_d, "emacsclient", ["-c", "-n"],       className =? "Emacs")
  , (xK_f, "zathura", [],                     className =? "Zathura")
  , (xK_o, "spotify", [],                     className =? "Spotify")
  , (xK_r, "transmission-remote-gtk", [],     className =? "Transmission-remote-gtk")
  , (xK_p, "pcmanfm", [],                     className =? "Pcmanfm")
  , (xK_t, "lxtask", [],                      className =? "Lxtask")
  , (xK_a, "urxvt", ["-e", "tmux", "attach"], title =? "tmux")]


