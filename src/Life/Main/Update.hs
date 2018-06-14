-- | Functions to update your life.

module Life.Main.Update
       ( lifeAdd
       ) where

import Path (Path, Rel, dirname, filename, toFilePath)
import Path.IO (doesDirExist, doesFileExist, getHomeDir, makeRelative, resolveDir, resolveFile)

import Life.Configuration (LifeConfiguration, lifePath, parseGlobalLife, singleDirConfig,
                           singleFileConfig, writeGlobalLife)
import Life.Github (Owner (..), updateDotfilesRepo)
import Life.Main.Init (lifeInit)
import Life.Message (chooseYesNo, errorMessage, infoMessage, promptNonEmpty, skipMessage,
                     warningMessage)
import Life.Shell (relativeToHome)

-- | Add path to existing life-configuration file.
lifeAdd :: FilePath -> IO ()
lifeAdd path = do
    -- TODO: check for dotfiles existence

    -- check for .life existence
    lifeFilePath  <- relativeToHome lifePath
    unlessM (doesFileExist lifeFilePath) $ do
        warningMessage ".life file doesn't exist"
        toInit <- chooseYesNo "Would you like to proceed initialization process?"
        if toInit
            then do
                infoMessage "Initialization process starts.."
                skipMessage "Insert your GitHub username:"
                owner <- promptNonEmpty
                lifeInit $ Owner owner
            else errorMessage "Aborting life-add command." >> exitFailure

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
