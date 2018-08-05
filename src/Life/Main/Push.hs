{-# LANGUAGE TupleSections #-}

-- | Functions to update remote repository

module Life.Main.Push
       ( lifePush
       ) where

import Lens.Micro.Platform ((^.))
import Path (Abs, Path, Rel, toFilePath, (</>))
import Path.IO (doesDirExist, doesFileExist, removeDirRecur, removeFile)

import Life.Configuration (LifeConfiguration (..), directories, files, lifeConfigMinus,
                           parseHomeLife, parseRepoLife)
import Life.Github (Branch(..), updateDotfilesRepo, withSynced)
import Life.Main.Init (lifeInitQuestion)
import Life.Message (abortCmd)
import Life.Shell (LifeExistence (..), relativeToHome, repoName, whatIsLife)
import Life.Validation (Validation (..))

import qualified Data.Set as Set
import qualified Data.Text as Text

lifePush :: IO ()
lifePush = whatIsLife >>= \case
    OnlyRepo _ -> abortCmd "push" ".life file doesn't exist"
    OnlyLife _ -> abortCmd "push" "dotfiles file doesn't exist"
    NoLife     -> lifeInitQuestion "push" pushProcess
    Both _ _   -> withSynced (Branch "master") pushProcess
  where
    pushProcess :: IO ()
    pushProcess = do
        -- check that all from .life exist
        globalConf <- parseHomeLife
        checkLife globalConf >>= \case
            Failure msgs -> abortCmd "push" $ "Following files/directories are missing:\n"
                                           <> Text.intercalate "\n" msgs
            Success _ -> do
                -- first, find the difference between repo .life and global .life
                repoConf <- parseRepoLife
                let removeConfig = lifeConfigMinus repoConf globalConf
                -- delete all redundant files from local dotfiles
                removeAll removeConfig

                -- copy from local files to repo including .life
                -- commmit & push
                updateDotfilesRepo "Push updates" globalConf


    -- | checks if all the files/dirs from global .life exist.
    checkLife :: LifeConfiguration -> IO (Validation [Text] LifeConfiguration)
    checkLife lf = do
        eFiles <- traverse (withExist doesFileExist) $ Set.toList (lf ^. files)
        eDirs  <- traverse (withExist doesDirExist) $ Set.toList (lf ^. directories)
        pure $ LifeConfiguration
            <$> checkPaths eFiles
            <*> checkPaths eDirs
      where
        withExist :: (Path Abs f -> IO Bool) -> Path Rel f -> IO (Path Rel f, Bool)
        withExist doesExist path = (path,) <$> (relativeToHome path >>= doesExist)

        checkPaths :: [(Path Rel f, Bool)] -> Validation [Text] (Set (Path Rel f))
        checkPaths = fmap Set.fromList . traverse checkPath

        checkPath :: (Path Rel t, Bool) -> Validation [Text] (Path Rel t)
        checkPath (f, is) = if is then Success f else Failure [toText (toFilePath f)]

    -- | removes all redundant files from repo folder.
    removeAll :: LifeConfiguration -> IO ()
    removeAll conf = do
        for_ (conf ^. files) $ \f ->
            relativeToHome (repoName </> f) >>= removeFile
        for_ (conf ^. directories) $ \d ->
            relativeToHome (repoName </> d) >>= removeDirRecur
