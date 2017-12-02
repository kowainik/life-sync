-- | Contains function to create repository.

module Life.Main.Init
       ( lifeInit
       ) where

import Universum

import Path.IO (withCurrentDir)

import Life.Github (Owner, Repo (Repo), createRepository, repoName)
import Life.Shell (createDirInHome)

-- TODO: use list of some predefined files and directories
lifeInit :: Owner -> IO ()
lifeInit owner = do
    repoPath <- createDirInHome repoName
    withCurrentDir repoPath $
        createRepository owner (Repo "dotfiles")
