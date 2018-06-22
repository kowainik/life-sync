{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeApplications #-}

-- | Functions to remove from your life.

module Life.Main.Remove
       ( lifeRemove
       ) where

import Path (Path, Rel, parseRelDir, parseRelFile)
import Path.IO (doesFileExist, removeDirRecur, removeFile)

import Life.Configuration (LifeConfiguration, directories, files, lifePath, parseGlobalLife,
                           writeGlobalLife)
import Life.Github (doesRepoExist, removeFromRepo)
import Life.Message (errorMessage, warningMessage)
import Life.Shell (relativeToHome)

import qualified Data.Set as Set

-- | Remove path from existing life-configuration file.
lifeRemove :: FilePath -> IO ()
lifeRemove path = do
    -- check for .life existence
    isLifeFile <- doesFileExist =<< relativeToHome lifePath
    -- check for dotfiles existence
    isDotDir   <- doesRepoExist

    case (isLifeFile, isDotDir) of
        -- if one of them is missing -- abort
        (True, False)  -> abortLifeRemove "dotfiles/ directory doesn't exist"
        (False, True)  -> abortLifeRemove ".life file doesn't exist"
        (False, False) -> abortLifeRemove ".life and docfiles/ do not exist"
         -- actual life remove process
        (True, True)   -> do
            let filePath = parseRelFile @Maybe path
            let dirPath  = parseRelDir  @Maybe path

            case (filePath, dirPath) of
                (Nothing, Nothing) -> abortLifeRemove $ toText path  <> " is neither file nor directory"
                (Just relFile, Nothing) -> resolveConfiguration files removeFile relFile
                (Nothing, Just relDir) -> resolveConfiguration directories removeDirRecur relDir
                _ -> error "File and dir at the same time"
  where
    abortLifeRemove :: Text -> IO ()
    abortLifeRemove message = do
        warningMessage message
        errorMessage "Aborting 'life remove' command."
        exitFailure


resolveConfiguration :: Lens' LifeConfiguration (Set (Path Rel t))
                     -> (Path Rel t -> IO ()) -- ^ function to remove object
                     -> Path Rel t
                     -> IO ()
resolveConfiguration confLens removeFun path = do
    configuration <- parseGlobalLife

    let newConfiguration = configuration & confLens %~ Set.delete path
    writeGlobalLife newConfiguration

    removeFromRepo removeFun path
