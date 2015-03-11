module XMonad.Config.Soft.Actions
  (runCompact, centerWindow, compact) where

import XMonad (X, Window, ManageHook, runQuery)
import XMonad.Operations (windows)
import XMonad.StackSet (StackSet)
import XMonad.Hooks.ManageHelpers (doCenterFloat)

import Control.Monad ((>=>))
import Data.Monoid (appEndo)

action :: ManageHook -> Window -> X ()
action h = runQuery h >=> windows . appEndo

centerWindow :: Window -> X ()
centerWindow = action doCenterFloat

-- TODO: Implement
runCompact :: X ()
runCompact = return ()

compact :: StackSet i l a s sd -> StackSet i l a s sd
compact = undefined
