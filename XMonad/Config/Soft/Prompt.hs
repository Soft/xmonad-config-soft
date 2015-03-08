module XMonad.Config.Soft.Prompt
  (Prompter(..), pipePrompt, rofi, internalPrompt)
  where

import Data.Default (def)
import Data.List (intercalate)

import XMonad (X)
import XMonad.Prompt (XPConfig(..))
import XMonad.Actions.Commands (defaultCommands, runCommand')
import XMonad.Util.Run (runProcessWithInput)

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

data Prompter = Prompter
  { completions :: X [String]
  , execute :: String -> X () }

pipePrompt :: FilePath -> [String] -> Prompter -> X ()
pipePrompt bin args p = do
  alts <- completions p
  choice <- runProcessWithInput bin args (intercalate "\n" alts)
  execute p (init choice)

rofi :: Prompter -> X ()
rofi = pipePrompt "rofi" ["-dmenu"]

internalPrompt :: Prompter
internalPrompt = Prompter (fmap (fmap fst) defaultCommands) runCommand'
