{-# LANGUAGE Rank2Types #-}

-- | Functions to remove from your life.

module Life.Main.Remove
       ( lifeRemove
       ) where

import Lens.Micro.Platform (Lens', (%~))
import Path (Abs, Path, Rel)
import Path.IO (getHomeDir, makeRelative, removeDirRecur, removeFile, resolveDir, resolveFile)

import Life.Configuration (LifeConfiguration, LifePath (..), directories, files, parseHomeLife,
                           writeGlobalLife)
import Life.Github (Branch(..), removeFromRepo, withSynced)
import Life.Message (abortCmd, warningMessage)
import Life.Shell (LifeExistence (..), whatIsLife)

import qualified Data.Set as Set

-- | Remove path from existing life-configuration file.
lifeRemove :: LifePath -> IO ()
lifeRemove lPath = whatIsLife >>= \case
    -- if one of them is missing -- abort
    NoLife -> abortCmd "remove" ".life and docfiles/ do not exist"
    OnlyLife _ -> abortCmd "remove" "dotfiles/ directory doesn't exist"
    OnlyRepo _ -> abortCmd "remove" ".life file doesn't exist"
    -- actual life remove process
    Both _ _ -> withSynced (Branch "master") $ do
        homeDirPath  <- getHomeDir
        case lPath of
            (File path) -> do
                filePath <- resolveFile homeDirPath path >>= makeRelative homeDirPath
                resolveConfiguration files removeFile filePath
            (Dir path)  -> do
                dirPath <- resolveDir homeDirPath path >>= makeRelative homeDirPath
                resolveConfiguration directories removeDirRecur dirPath

resolveConfiguration :: Lens' LifeConfiguration (Set (Path Rel t))
                     -> (Path Abs t -> IO ()) -- ^ function to remove object
                     -> Path Rel t
                     -> IO ()
resolveConfiguration confLens removeFun path = do
    configuration <- parseHomeLife

    let newConfiguration = configuration & confLens %~ Set.delete path
    if configuration == newConfiguration
    then warningMessage "File or directory is not in tracked" >> exitFailure
    else do
        writeGlobalLife newConfiguration
        removeFromRepo removeFun path
