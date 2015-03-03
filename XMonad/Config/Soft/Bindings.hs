module XMonad.Config.Soft.Bindings
  (keyMap) where

import qualified Data.Map as M

import XMonad (XConfig, KeyMask, KeySym, X, sendMessage)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace, tagToEmptyWorkspace)
import XMonad.Layout.Groups.Helpers (swapMaster)
import XMonad.Layout.BinarySpacePartition (ResizeDirectional(..), Direction2D(..), Rotate(..), Swap(..))
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Prompt (XPConfig)

import XMonad.Config.Soft.Actions
import XMonad.Config.Soft.Prompt

keyMap :: XPConfig -> XConfig a -> M.Map (KeyMask, KeySym) (X ())
keyMap xpc = flip mkKeymap
  [ ("M-0", viewEmptyWorkspace)
  , ("M-S-0", tagToEmptyWorkspace)
  , ("M-<Backspace>", toggleWS)
  , ("M-S-m", swapMaster)
  , ("M-x", internalPrompt xpc)
  , ("M-c", runCompact)

  -- Keybindings for BinarySpacePartition
  , ("C-M-l", sendMessage $ MoveSplit R)
  , ("C-M-h", sendMessage $ MoveSplit L)
  , ("C-M-j", sendMessage $ MoveSplit D)
  , ("C-M-k", sendMessage $ MoveSplit U)
  , ("M-r", sendMessage Rotate)
  , ("M-s", sendMessage Swap) ]


