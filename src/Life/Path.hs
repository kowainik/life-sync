{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains utility functions to work with shell.
-}

module Life.Path
    ( -- * Constants
      lifePath
    , repoName

      -- * Functions
    , LifeExistence (..)
    , createDirInHome
    , relativeToHome
    , whatIsLife
    ) where

import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, (</>))
import Path.IO (createDirIfMissing, doesDirExist, doesFileExist, getHomeDir)


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
