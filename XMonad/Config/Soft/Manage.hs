module XMonad.Config.Soft.Manage where

import Control.Applicative ((<$>))
import Data.Monoid ((<>), mconcat)

import XMonad (ManageHook)
import XMonad.ManageHook ((-->), (=?), className)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)

manage :: [String] -> ManageHook
manage floats = manageDocks <> fullscreen <> handleFloats
  where
    fullscreen = isFullscreen --> doFullFloat
    handleFloats = mconcat $ ((--> doCenterFloat) . (=?) className) <$> floats

