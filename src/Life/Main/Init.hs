-- | Contains function to create repository.

module Life.Main.Init
       ( lifeInit
       ) where

import Path.IO (copyFile, doesFileExist)

import Life.Configuration (lifePath, singleFileConfig, writeGlobalLife)
import Life.Github (Owner, Repo (Repo), createRepository, doesRepoExist, insideRepo, repoName)
import Life.Message (chooseYesNo, errorMessage, infoMessage, warningMessage)
import Life.Shell (createDirInHome, relativeToHome)

lifeInit :: Owner -> IO ()
lifeInit owner = do
    -- create initial life configuration
    lifeFilePath  <- relativeToHome lifePath
    -- check for .life existence
    isFile <- doesFileExist lifeFilePath
    if isFile
        then do
            warningMessage ".life file is already exist."
            useIt <- chooseYesNo "Would you like to use it?"
            unless useIt writeConf
        else writeConf

    -- check for dotfiles existence
    whenM doesRepoExist $
        errorMessage "dotfiles folder already exist" >> exitFailure

    -- create dotfiles repository
    () <$ createDirInHome repoName
    insideRepo $ do
        -- TODO: use list of some predefined files and directories
        copyFile lifeFilePath lifePath
        createRepository owner (Repo "dotfiles")
  where
    writeConf :: IO ()
    writeConf = do
        infoMessage "Writing .life configuration file"
        let lifeConfig = singleFileConfig lifePath
        writeGlobalLife lifeConfig
