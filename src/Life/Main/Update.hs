-- | Functions to update your life.

module Life.Main.Update
       ( lifeAdd
       ) where

import Path (Path, Rel, dirname, filename, toFilePath)
import Path.IO (doesDirExist, doesFileExist, getHomeDir, makeRelative, resolveDir, resolveFile)

import Life.Configuration (LifeConfiguration, lifePath, parseGlobalLife, singleDirConfig,
                           singleFileConfig, writeGlobalLife)
import Life.Github (Owner (..), doesRepoExist, updateDotfilesRepo)
import Life.Main.Init (lifeInit)
import Life.Message (chooseYesNo, errorMessage, infoMessage, promptNonEmpty, skipMessage,
                     warningMessage)
import Life.Shell (relativeToHome)

-- | Add path to existing life-configuration file.
lifeAdd :: FilePath -> IO ()
lifeAdd path = do
    -- check for .life existence
    isLifeFile <- doesFileExist =<< relativeToHome lifePath
    -- check for dotfiles existence
    isDotDir   <- doesRepoExist

    case (isLifeFile, isDotDir) of
        -- actual life add process
        (True, True)  -> addingProcess
        -- if one of them is missing -- abort
        (True, False) -> warningMessage "dotfiles/ directory doesn't exist" >> abortLifeAdd
        (False, True) -> warningMessage ".life file doesn't exist" >> abortLifeAdd
        -- if both .life and dotfiles doesn't exist go to init process
        (False, False) -> do
            warningMessage ".life file and dotfiles/ do not exist"
            toInit <- chooseYesNo "Would you like to proceed initialization process?"
            if toInit
                then do
                    infoMessage "Initialization process starts.."
                    skipMessage "Insert your GitHub username:"
                    owner <- promptNonEmpty
                    lifeInit $ Owner owner
                else abortLifeAdd
            addingProcess
  where
    abortLifeAdd :: IO ()
    abortLifeAdd = errorMessage "Aborting life-add command." >> exitFailure

    addingProcess :: IO ()
    addingProcess = do
        homeDirPath <- getHomeDir
        filePath    <- resolveFile homeDirPath path
        dirPath     <- resolveDir  homeDirPath path

        -- check whether `path` is file or dir
        isFile <- doesFileExist filePath
        isDir  <- doesDirExist  dirPath

        if isFile then do
            relativeFile <- makeRelative homeDirPath filePath
            resolveConfiguration singleFileConfig filename relativeFile
        else if isDir then do
            relativeDir <- makeRelative homeDirPath dirPath
            resolveConfiguration singleDirConfig dirname relativeDir
        else
            putTextLn $ toText path <> " is neither file nor directory or just doesn't exist"


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
