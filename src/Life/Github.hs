{-# LANGUAGE TemplateHaskell #-}
-- | Utilities to work with GitHub repositories using "hub".

module Life.Github
       ( Owner (..)
       , Repo  (..)

         -- * Repository utils
       , repoName
       , insideRepo

         -- * Repository manipulation commands
       , createRepository
       , updateDotfilesRepo
       ) where

import Universum

import Path (Abs, Dir, File, Path, Rel, mkRelDir, (</>))
import Path.IO (copyDirRecur, copyFile, getHomeDir, withCurrentDir)

import Life.Configuration (LifeConfiguration (..))
import Life.Shell (relativeToHome)

newtype Owner = Owner { getOwner :: Text } deriving (Show)
newtype Repo  = Repo  { getRepo  :: Text } deriving (Show)

----------------------------------------------------------------------------
-- VSC commands
----------------------------------------------------------------------------

-- | Make a commit and push it.
pushka :: Text -> IO ()
pushka commitMsg = do
    "git" ["add", "."]
    "git" ["commit", "-m", commitMsg]
    "git" ["push", "-u", "origin", "master"]

-- | Creates repository on GitHub inside given folder.
createRepository :: Owner -> Repo -> IO ()
createRepository (Owner owner) (Repo repo) = do
    let description = ":computer: Configuration files"
    "git" ["init"]
    "hub" ["create", "-d", description, owner <> "/" <> repo]
    pushka "Create the project"

----------------------------------------------------------------------------
-- dotfiles workflow
----------------------------------------------------------------------------

-- | Default repository name for life configuration files.
repoName :: Path Rel Dir
repoName = $(mkRelDir "dotfiles/")

-- | Executes action with 'repoName' set as pwd.
insideRepo :: (MonadIO m, MonadMask m) => m a -> m a
insideRepo action = do
    repoPath <- relativeToHome repoName
    withCurrentDir repoPath action

-- | Commits all changes inside 'repoName' and pushes to remote.
pushRepo :: Text -> IO ()
pushRepo = insideRepo . pushka

----------------------------------------------------------------------------
-- File manipulation
----------------------------------------------------------------------------

updateDotfilesRepo :: Text -> LifeConfiguration -> IO ()
updateDotfilesRepo commitMsg LifeConfiguration{..} = do
    copyFiles (toList lifeConfigurationFiles)
    copyDirs  (toList lifeConfigurationDirectories)
    pushRepo commitMsg

-- | Copy files to repository and push changes to remote repository.
copyFiles :: [Path Rel File] -> IO ()
copyFiles = copyPathList copyFile

-- | Copy dirs to repository.
copyDirs :: [Path Rel Dir] -> IO ()
copyDirs = copyPathList copyDirRecur

copyPathList :: (Path Abs t -> Path Abs t -> IO ())
             -- ^ Copying action
             -> [Path Rel t]
             -- ^ List of paths to copy
             -> IO ()
copyPathList copyAction pathList = do
    homeDir    <- getHomeDir
    let repoDir = homeDir </> repoName

    for_ pathList $ \entryPath -> do
        let copySource      = homeDir </> entryPath
        let copyDestination = repoDir </> entryPath
        copyAction copySource copyDestination
