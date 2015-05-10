{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
module XMonad.Config.Soft.Layouts where

import Data.Default (def)
import Data.Typeable (Typeable)
import Graphics.X11 (Rectangle(..))

import XMonad (Full(..), runLayout, (|||), Message, fromMessage)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Decoration (Theme(..), Decoration, DefaultShrinker)
-- import XMonad.Layout.LayoutBuilder (layoutN, layoutAll, relBox)
import XMonad.Layout.LayoutModifier (LayoutModifier(..), ModifiedLayout (..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.Tabbed (TabbedDecoration(..), tabbed, shrinkText) 
import XMonad.StackSet (Workspace(..), Stack(..))

import XMonad.Config.Soft.Resources

-- twoPanels = layoutN 1
--             (relBox 0 0 0.5 1)
--             (Just $ relBox 0 0 1 1)
--             Full
--             (layoutAll (relBox 0.5 0 1 1) simpleTabbed)

tabs :: Eq a => Theme -> ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest a
tabs = tabbed shrinkText

bspLayout = spacing 20 emptyBSP

layout theme = avoidStruts $ smartBorders (bspLayout ||| Full)

instance IsXRSettings Theme where
  fromXRSettings m = do
    ac <- get "activeColor"
    ic <- get "inactiveColor"
    uc <- get "urgentColor"
    ab <- get "activeBorderColor"
    ib <- get "inactiveBorderColor"
    ub <- get "urgentBorderColor"
    at <- get "activeTextColor"
    it <- get "inactiveTextColor"
    ut <- get "urgentTextColor"
    fnt <- get "fontName"
    return $ def { activeColor = ac
                 , inactiveColor = ic
                 , urgentColor = uc
                 , activeBorderColor = ab
                 , inactiveBorderColor = ib
                 , urgentBorderColor = ub
                 , activeTextColor= at
                 , inactiveTextColor = it
                 , urgentTextColor = ut
                 , fontName = fnt }
    where
      get = setting m

-- This is mostly borrowed from https://github.com/egasimus/xmonad-equalspacing/blob/master/EqualSpacing.hs
-- Removed the automatic scaling and added 'smart' behaviour

data SpacingMsg = ToggleSpacing
                deriving (Typeable)

instance Message SpacingMsg

spacing g = ModifiedLayout (Spacing g True)

data Spacing a = Spacing
  { gap :: Int
  , enabled :: Bool
  } deriving (Show, Read)

instance LayoutModifier Spacing a where
  modifyLayout _ w@(Workspace _ _ (Just (Stack _ [] []))) r = runLayout w r
  modifyLayout m workspace screen = runLayout workspace $ shrinkScreen m screen
  pureModifier _ _ _ [x] = ([x], Nothing)
  pureModifier m _ _ windows = (map shrink windows, Nothing)
    where shrink (a, rect) = (a, shrinkWindow m rect)
  pureMess l msg
    | Just ToggleSpacing <- fromMessage msg = Just $ l { enabled = not (enabled l) }
    | otherwise = Nothing

shrinkScreen (Spacing g True) (Rectangle x y w h) =
  Rectangle x y (w - fromIntegral g) (h - fromIntegral g)
shrinkScreen (Spacing _ False) r = r

shrinkWindow (Spacing g True) (Rectangle x y w h) =
  Rectangle (x + fromIntegral g) (y + fromIntegral g) (w - fromIntegral g) (h - fromIntegral g)
shrinkWindow (Spacing _ False) r = r


