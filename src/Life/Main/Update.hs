-- | Functions to update your life.

module Life.Main.Update
       ( lifeAdd
       ) where

import Universum

import Path.IO (doesDirExist, doesFileExist, getHomeDir, makeRelative, resolveDir, resolveFile)

import Life.Configuration (parseGlobalLife, singleDirConfig, singleFileConfig, writeGlobalLife)
import Life.Github (updateDotfilesRepo)

-- | Add path to existing life-configuration file.
lifeAdd :: FilePath -> IO ()
lifeAdd path = do
    -- TODO: check for .life existence
    -- TODO: check for dotfiles existence

    homeDirPath <- getHomeDir
    filePath    <- resolveFile homeDirPath path
    dirPath     <- resolveDir  homeDirPath path

    -- check whether `path` is file or dir
    isFile <- doesFileExist filePath
    isDir  <- doesDirExist  dirPath

    configuration <- parseGlobalLife

    toAdd <- if isFile then do
                 relativeFile <- makeRelative homeDirPath filePath
                 pure $ singleFileConfig relativeFile
             else if isDir then do
                 relativeDir <- makeRelative homeDirPath dirPath
                 pure $ singleDirConfig relativeDir
             else do
                 putText $ toText path <> " is neither file nor directory or just doesn't exist"
                 pure mempty

    let newConfiguration = configuration <> toAdd
    writeGlobalLife newConfiguration
    updateDotfilesRepo newConfiguration
