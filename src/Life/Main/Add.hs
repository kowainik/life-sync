-- | Functions to add file/directory to your life.

module Life.Main.Add
       ( lifeAdd
       ) where

import Path (Path, Rel, dirname, filename, toFilePath)
import Path.IO (doesDirExist, doesFileExist, getHomeDir, makeRelative, resolveDir, resolveFile)

import Life.Configuration (LifeConfiguration, LifePath (..), parseGlobalLife, singleDirConfig,
                           singleFileConfig, writeGlobalLife)
import Life.Github (Owner (..), updateDotfilesRepo)
import Life.Main.Init (lifeInit)
import Life.Message (abortCmd, chooseYesNo, errorMessage, infoMessage, promptNonEmpty, skipMessage,
                     warningMessage)
import Life.Shell (LifeExistence (..), whatIsLife)

-- | Add path to existing life-configuration file.
lifeAdd :: LifePath -> IO ()
lifeAdd lPath = whatIsLife >>= \case
    -- actual life add process
    Both _ _ -> addingProcess

    -- if one of them is missing -- abort
    OnlyRepo _ -> abortCmd "add" ".life file doesn't exist"
    OnlyLife _ -> abortCmd "add" "dotfiles/ directory doesn't exist"

    -- if both .life and dotfiles doesn't exist go to init process
    NoLife -> do
        warningMessage ".life file and dotfiles/ do not exist"
        toInit <- chooseYesNo "Would you like to proceed initialization process?"
        if toInit then do
            infoMessage "Initialization process starts.."
            skipMessage "Insert your GitHub username:"
            owner <- promptNonEmpty
            lifeInit $ Owner owner
            addingProcess
        else abortCmd "add" "Can't execute 'life add' if '~/.life' file is not initialized"
  where
    addingProcess :: IO ()
    addingProcess = do
        homeDirPath <- getHomeDir
        case lPath of
            (File path) -> do
                filePath <- resolveFile homeDirPath path
                whenM (doesFileExist filePath) $ do
                    relativeFile <- makeRelative homeDirPath filePath
                    resolveConfiguration singleFileConfig filename relativeFile

            (Dir path)  -> do
                dirPath <- resolveDir homeDirPath path
                whenM (doesDirExist dirPath) $ do
                    relativeDir <- makeRelative homeDirPath dirPath
                    resolveConfiguration singleDirConfig dirname relativeDir

            -- We didn't find the file
        errorMessage "The file/directory doesn't exist" >> exitFailure


resolveConfiguration :: (Path Rel t -> LifeConfiguration)
                     -> (Path Rel t -> Path Rel t)
                     -> Path Rel t
                     -> IO ()
resolveConfiguration configBuilder pathName path = do
    configuration <- parseGlobalLife
    let newConfiguration = configuration <> configBuilder path
    writeGlobalLife newConfiguration

    let pathTextName = toText $ toFilePath $ pathName path
    let commitMsg    = "Add: " <> pathTextName
    updateDotfilesRepo commitMsg newConfiguration
    exitSuccess
