module XMonad.Config.Soft.Resources
  (XRSettings, XRName, XRValue, IsXRSettings(..), settings, setting, loadSettings)
  where

import Data.Default (def)
import Control.Applicative((<$>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Map.Strict as M

import Graphics.X11.Xlib.Display (openDisplay)
import Graphics.X11.Xlib.Misc (rmInitialize)
import Graphics.X11.XRM

import XMonad (XConfig(..))
import XMonad.Layout.Tabbed (Theme(..))
import XMonad.Prompt (XPConfig(..))

type XRName = String
type XRValue = String
type XRSettings = M.Map XRName XRValue

class IsXRSettings a where
  fromXRSettings :: XRSettings -> Maybe a

settings :: XRSettings
settings = M.fromList
  [ ("terminal", terminal conf)
  , ("normalBorderColor", normalBorderColor conf)
  , ("focusedBorderColor", focusedBorderColor conf)
  -- Decorations
  , ("activeColor", activeColor theme)
  , ("inactiveColor", inactiveColor theme)
  , ("urgentColor", urgentColor theme)
  , ("activeBorderColor", activeBorderColor theme)
  , ("inactiveBorderColor", inactiveBorderColor theme)
  , ("urgentBorderColor", urgentBorderColor theme)
  , ("activeTextColor", activeTextColor theme)
  , ("inactiveTextColor", inactiveTextColor theme)
  , ("urgentTextColor", urgentTextColor theme)
  , ("fontName", fontName theme)
  -- XPConfig
  , ("promptFont", font xpc)
  , ("promptBgColor", bgColor xpc)
  , ("promptFgColor", fgColor xpc)
  , ("promptFgHlight", fgHLight xpc)
  , ("promptBgHlight", bgHLight xpc)
  , ("promptBorderColor", borderColor xpc)
  , ("promptDefaultText", defaultText xpc)
  -- Floats
  , ("floating", "")]
  where
    theme = def
    conf = def
    xpc = def

setting :: XRSettings -> XRName -> Maybe XRValue
setting s n = M.lookup ("XMonad." ++ n) s

loadSettings :: XRSettings -> IO (Maybe XRSettings)
loadSettings defs = do
  display <- openDisplay ""
  rmInitialize
  runMaybeT $ do 
    rm <- MaybeT $ resourceManagerString display
    db <- MaybeT $ rmGetStringDatabase rm
    M.fromList <$> mapM (uncurry $ getRes db) (M.assocs defs)
  where
    prefix = ("XMonad." ++)
    getRes db s v = lift $ do
      res <- rmGetResource db (prefix s) (prefix s)
      str <- maybe (return v) (rmValue . snd) res
      return (prefix s, str)

