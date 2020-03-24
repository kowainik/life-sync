{-# LANGUAGE Rank2Types #-}

{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to add file/directory to your life.
-}

module Life.Main.Add
    ( lifeAdd
    ) where

import Path (Abs, Dir, File, Path, Rel, parent, toFilePath, (</>))
import Path.IO (copyDirRecur, copyFile, doesDirExist, doesFileExist, ensureDir, getHomeDir,
                makeRelative, resolveDir, resolveFile)
import Relude.Extra.Lens (Lens', (%~))

import Life.Configuration (LifeConfiguration, directoriesL, filesL, parseHomeLife, writeGlobalLife)
import Life.Core (LifePath (..), master)
import Life.Github (addToRepo, withSynced)
import Life.Main.Init (lifeInitQuestion)
import Life.Message (abortCmd, errorMessage, infoMessage, warningMessage)
import Life.Path (LifeExistence (..), relativeToHome, repoName, whatIsLife)

import qualified Data.Set as Set


-- | Add path to existing life-configuration file.
lifeAdd :: LifePath -> IO ()
lifeAdd lPath = whatIsLife >>= \case
    -- actual life add process
    Both _ _ -> withSynced master addingProcess

    -- if one of them is missing -- abort
    OnlyRepo _ -> abortCmd "add" ".life file doesn't exist"
    OnlyLife _ -> abortCmd "add" "dotfiles/ directory doesn't exist"

    -- if both .life and dotfiles doesn't exist go to init process
    NoLife -> lifeInitQuestion "add" addingProcess
  where
    addingProcess :: IO ()
    addingProcess = do
        homeDirPath <- getHomeDir
        case lPath of
            (File path) -> do
                filePath <- resolveFile homeDirPath path
                whenM (doesFileExist filePath) $ do
                    relativeFile <- makeRelative homeDirPath filePath
                    resolveConfiguration filesL checkEqualFiles copyFileWithDir relativeFile

            (Dir path)  -> do
                dirPath <- resolveDir homeDirPath path
                whenM (doesDirExist dirPath) $ do
                    relativeDir <- makeRelative homeDirPath dirPath
                    resolveConfiguration directoriesL checkEqualDirs copyDirRecur relativeDir

            -- We didn't find the file
        errorMessage "The file/directory doesn't exist" >> exitFailure

resolveConfiguration
    :: Lens' LifeConfiguration (Set (Path Rel t))
    -> (Path Rel t -> IO Bool)
    -> (Path Abs t -> Path Abs t -> IO ())
    -> Path Rel t
    -> IO ()
resolveConfiguration confLens checkContent copyFun path = do
    configuration <- parseHomeLife
    let newConfiguration = configuration & confLens %~ Set.insert path

    isSameAsInRepo <- checkContent path
    if isSameAsInRepo
    then do
        let pathText = toText $ toFilePath path
        infoMessage $ "Path " <> pathText <> " is already latest version in repository"
    else do
        writeGlobalLife newConfiguration
        addToRepo copyFun path

    exitSuccess

checkEqualFiles :: Path Rel File -> IO Bool
checkEqualFiles path = do
    homeFilePath <- relativeToHome path
    repoFilePath <- relativeToHome (repoName </> path)

    isRepoFile <- doesFileExist repoFilePath
    if isRepoFile
    then do
        originContent <- readFileLBS $ toFilePath homeFilePath
        repoContent <- readFileLBS $ toFilePath repoFilePath

        pure $ originContent == repoContent
    else pure False

checkEqualDirs :: Path Rel Dir -> IO Bool
checkEqualDirs _ = do
    warningMessage "TODO: check directories to be equal"
    pure False

-- | Just like 'copyFile' but also creates directory for the second file.
copyFileWithDir :: Path Abs File -> Path Abs File -> IO ()
copyFileWithDir from to = ensureDir (parent to) >> copyFile from to
