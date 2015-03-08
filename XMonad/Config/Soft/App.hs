module XMonad.Config.Soft.App
  (main, config, workspaces') where

import Control.Applicative ((<$>))
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

import XMonad (xmonad, XConfig(..), mod4Mask)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Layout.Decoration (Theme)
-- import XMonad.Layout.IndependentScreens (withScreens)

import XMonad.Config.Soft.Resources
import XMonad.Config.Soft.Startup
import XMonad.Config.Soft.Layouts
import XMonad.Config.Soft.Logging
import XMonad.Config.Soft.Manage
import XMonad.Config.Soft.Bindings

workspaces' :: [String]
workspaces' = show <$> [1..9]
-- workspaces' = withScreens 2 $ show <$> [1..9]

config res pipe = do
  term <- setting res "terminal"
  normal <- setting res "normalBorderColor"
  focused <- setting res "focusedBorderColor"
  floats <- setting res "floating"
  theme <- fromXRSettings res
  -- prompt <- fromXRSettings res
  let conf = def { terminal = term
                 , normalBorderColor = normal
                 , focusedBorderColor = focused
                 , borderWidth = 2
                 , modMask = mod4Mask
                 , workspaces = workspaces'
                 , layoutHook = layout (theme :: Theme)
                 , manageHook = manage (splitOn "," floats)
                 , handleEventHook = fullscreenEventHook
                 , logHook = logger pipe}
  return
    $ ewmh
    $ additionalKeys conf (M.assocs $ keyMap conf)

main :: IO ()
main = do
  (path, pipe) <- createPipe
  executeStartupFile [path]
  res <- fromMaybe settings <$> loadSettings settings
  maybe (putStrLn "Failed to load config") xmonad (config res pipe)

