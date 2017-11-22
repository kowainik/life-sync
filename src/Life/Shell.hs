{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module contains utility functions to work with shell.

module Life.Shell
       ( createDirInHome
       ) where

import Universum

import Path (Abs, Dir, Path, Rel, (</>))
import Path.IO (createDirIfMissing, getHomeDir)
import System.Process (callCommand, showCommandForUser)

-- This is needed to be able to call commands by writing strings.
instance (a ~ Text, b ~ ()) => IsString ([a] -> IO b) where
    fromString cmd args = callCommand $ showCommandForUser cmd (map toString args)

-- | Creates directory with name "folder" under "~/folder".
createDirInHome :: Path Rel Dir -> IO (Path Abs Dir)
createDirInHome dirName = do
    homeDir <- getHomeDir
    let newDir = homeDir </> dirName
    newDir <$ createDirIfMissing False newDir
