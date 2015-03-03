module XMonad.Config.Soft.Prompt
  (internalPrompt)
  where

import Data.Default (def)

import XMonad (X)
import XMonad.Prompt (XPConfig(..))
import XMonad.Prompt.XMonad (xmonadPrompt)

import XMonad.Config.Soft.Resources

instance IsXRSettings XPConfig where
  fromXRSettings m = do
    fnt <- get "promptFont"
    bg <- get "promptBgColor"
    fg <- get "promptFgColor"
    fgh <- get "promptFgHlight"
    bgh <- get "promptBgHlight"
    bColor <- get "promptBorderColor"
    text <- get "promptDefaultText"
    return $ def { font = fnt
                 , bgColor = bg
                 , fgColor = fg
                 , fgHLight = fgh
                 , bgHLight = bgh
                 , borderColor = bColor
                 , defaultText = text }
    where
      get = setting m

internalPrompt :: XPConfig -> X ()
internalPrompt = xmonadPrompt

-- data Prompter = Prompter
--   { completions :: X [String]
--   , execute :: String -> X () }

-- externalPrompt :: FilePath -> Prompter
-- externalPrompt = undefined
