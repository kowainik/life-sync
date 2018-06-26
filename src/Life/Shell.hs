{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module contains utility functions to work with shell.

-- TODO: rename this module to Life.Path ?

module Life.Shell
       ( -- * Constants
         lifePath
       , repoName

         -- * Functions
       , LifeExistence (..)
       , createDirInHome
       , relativeToHome
       , whatIsLife
       , ($|)
       ) where

import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, (</>))
import Path.IO (createDirIfMissing, doesDirExist, doesFileExist, getHomeDir)
import System.Process (callCommand, readProcess, showCommandForUser)

----------------------------------------------------------------------------
-- Global constants
----------------------------------------------------------------------------

-- | Name for life configuration file.
lifePath :: Path Rel File
lifePath = $(mkRelFile ".life")

-- TODO: consistent naming with @lifePath@ ?
-- | Default repository name for life configuration files.
repoName :: Path Rel Dir
repoName = $(mkRelDir "dotfiles/")

----------------------------------------------------------------------------
-- Shell interface
----------------------------------------------------------------------------

-- This is needed to be able to call commands by writing strings.
instance (a ~ Text, b ~ ()) => IsString ([a] -> IO b) where
    fromString cmd args = callCommand $ showCommandForUser cmd (map toString args)

-- | Run shell command with given options and return stdout of executed command.
infix 5 $|
($|) :: FilePath -> [Text] -> IO String
cmd $| args = readProcess cmd (map toString args) ""

-- | Creates directory with name "folder" under "~/folder".
createDirInHome :: Path Rel Dir -> IO (Path Abs Dir)
createDirInHome dirName = do
    newDir <- relativeToHome dirName
    newDir <$ createDirIfMissing False newDir

-- | Creates path relative to home directory
relativeToHome :: MonadIO m => Path Rel t -> m (Path Abs t)
relativeToHome path = do
    homeDir <- getHomeDir
    pure $ homeDir </> path

data LifeExistence
    = NoLife
    | OnlyLife (Path Abs File)
    | OnlyRepo (Path Abs Dir)
    | Both (Path Abs File) (Path Abs Dir)

whatIsLife :: IO LifeExistence
whatIsLife = do
    lifeFile <- relativeToHome lifePath
    repoDir  <- relativeToHome repoName

    isFile <- doesFileExist lifeFile
    isDir  <- doesDirExist  repoDir

    pure $ case (isFile, isDir) of
        (False, False) -> NoLife
        (True, False)  -> OnlyLife lifeFile
        (False, True)  -> OnlyRepo repoDir
        (True, True)   -> Both lifeFile repoDir
