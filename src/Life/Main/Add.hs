{-# LANGUAGE Rank2Types #-}

-- | Functions to add file/directory to your life.

module Life.Main.Add
       ( lifeAdd
       ) where

import Path (Abs, Dir, File, Path, Rel, parent, toFilePath, (</>))
import Path.IO (copyDirRecur, copyFile, doesDirExist, doesFileExist, ensureDir, getHomeDir,
                makeRelative, resolveDir, resolveFile)

import Life.Configuration (LifeConfiguration, LifePath (..), directories, files, parseGlobalLife,
                           writeGlobalLife)
import Life.Github (Owner (..), addToRepo)
import Life.Main.Init (lifeInit)
import Life.Message (abortCmd, chooseYesNo, errorMessage, infoMessage, promptNonEmpty, skipMessage,
                     warningMessage)
import Life.Shell (LifeExistence (..), relativeToHome, repoName, whatIsLife)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set

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
                    resolveConfiguration files checkEqualFiles copyFileWithDir relativeFile

            (Dir path)  -> do
                dirPath <- resolveDir homeDirPath path
                whenM (doesDirExist dirPath) $ do
                    relativeDir <- makeRelative homeDirPath dirPath
                    resolveConfiguration directories checkEqualDirs copyDirRecur relativeDir

            -- We didn't find the file
        errorMessage "The file/directory doesn't exist" >> exitFailure

resolveConfiguration :: Lens' LifeConfiguration (Set (Path Rel t))
                     -> (Path Rel t -> IO Bool)
                     -> (Path Abs t -> Path Abs t -> IO ())
                     -> Path Rel t
                     -> IO ()
resolveConfiguration confLens checkContent copyFun path = do
    configuration <- parseGlobalLife
    let newConfiguration = configuration & confLens %~ Set.insert path

    isSameAsInRepo <- checkContent path
    if isSameAsInRepo then do
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
    if isRepoFile then do
        originContent <- LBS.readFile $ toFilePath homeFilePath
        repoContent <- LBS.readFile $ toFilePath repoFilePath

        pure $ originContent == repoContent
    else
        pure False


checkEqualDirs :: Path Rel Dir -> IO Bool
checkEqualDirs _ = do
    warningMessage "TODO: check directories to be equal"
    pure True

-- | Just like 'copyFile' but also creates directory for second file.
copyFileWithDir :: Path Abs File -> Path Abs File -> IO ()
copyFileWithDir from to = ensureDir (parent to) >> copyFile from to
