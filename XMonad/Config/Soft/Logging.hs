module XMonad.Config.Soft.Logging
  (createPipe, logger) where

import Data.Default (def)
import Data.Word (Word32)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), openFile, hPutStrLn, hFlush)
import System.Posix (createNamedPipe, ownerModes)
import System.Random (randomIO)

import XMonad (X, getXMonadDir)
import XMonad.Hooks.DynamicLog (PP(..), dynamicLogWithPP)

pipePath :: IO (FilePath)
pipePath = do
  dir <- getXMonadDir
  rand <- randomIO :: IO (Word32)
  return (dir </> "pipe." ++ show rand)

createPipe :: IO (FilePath, Handle)
createPipe = do
  pipe <- pipePath
  createNamedPipe pipe ownerModes
  handle <- openFile pipe ReadWriteMode
  return (pipe, handle)

logger :: Handle -> X ()
logger pipe = dynamicLogWithPP $ def { ppOutput = send }
  where
    send str = do hPutStrLn pipe str
                  hFlush pipe
