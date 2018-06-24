{-# LANGUAGE Rank2Types #-}

-- | Functions to remove from your life.

module Life.Main.Remove
       ( lifeRemove
       ) where

import Path (Path, Rel, parseRelDir, parseRelFile)
import Path.IO (removeDirRecur, removeFile)

import Life.Configuration (LifeConfiguration, directories, files, parseGlobalLife, writeGlobalLife)
import Life.Github (removeFromRepo)
import Life.Message (abortCmd, warningMessage)
import Life.Shell (LifeExistence (..), whatIsLife)

import qualified Data.Set as Set

-- | Remove path from existing life-configuration file.
lifeRemove :: FilePath -> IO ()
lifeRemove path = whatIsLife >>= \case
    -- if one of them is missing -- abort
    NoLife -> abortCmd "remove" ".life and docfiles/ do not exist"
    OnlyLife _ -> abortCmd "remove" "dotfiles/ directory doesn't exist"
    OnlyRepo _ -> abortCmd "remove" ".life file doesn't exist"
    -- actual life remove process
    Both _ _ -> do
        let filePath = parseRelFile @Maybe path
        let dirPath  = parseRelDir  @Maybe path

        case (filePath, dirPath) of
            (Nothing, Nothing) -> abortCmd "remove" $ toText path  <> " is neither file nor directory"
            (Just relFile, Nothing) -> resolveConfiguration files removeFile relFile
            (Nothing, Just relDir) -> resolveConfiguration directories removeDirRecur relDir
            _ -> error "File and dir at the same time"

resolveConfiguration :: Lens' LifeConfiguration (Set (Path Rel t))
                     -> (Path Rel t -> IO ()) -- ^ function to remove object
                     -> Path Rel t
                     -> IO ()
resolveConfiguration confLens removeFun path = do
    configuration <- parseGlobalLife

    let newConfiguration = configuration & confLens %~ Set.delete path
    if configuration == newConfiguration
    then warningMessage "File or directory is not in tracked" >> exitFailure
    else do
        writeGlobalLife newConfiguration
        removeFromRepo removeFun path
