module XMonad.Config.Soft.Startup where

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))

import XMonad.Util.Run (safeSpawn)

startupFile :: IO FilePath
startupFile = (</> ".xmonadrc") <$> getHomeDirectory

executeStartupFile :: [String] -> IO ()
executeStartupFile args = do
  path <- startupFile
  exists <- doesFileExist path
  when exists $ safeSpawn path args

