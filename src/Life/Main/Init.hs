-- | Contains function to create repository.

module Life.Main.Init
       ( lifeInit
       ) where

import Universum

import Path.IO (copyFile)

import Life.Configuration (lifePath, singleFileConfig, writeGlobalLife)
import Life.Github (Owner, Repo (Repo), createRepository, insideRepo, repoName)
import Life.Shell (createDirInHome, relativeToHome)

lifeInit :: Owner -> IO ()
lifeInit owner = do
    -- create initial life configuration
    -- TODO: check for .life existence
    lifeFilePath  <- relativeToHome lifePath
    let lifeConfig = singleFileConfig lifePath
    writeGlobalLife lifeConfig

    -- create dotfiles repository
    -- TODO: check for dotfiles existence
    () <$ createDirInHome repoName
    insideRepo $ do
        -- TODO: use list of some predefined files and directories
        copyFile lifeFilePath lifePath
        createRepository owner (Repo "dotfiles")
