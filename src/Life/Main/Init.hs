{-# LANGUAGE TemplateHaskell #-}

-- | Contains function to create repository.

module Life.Main.Init
       ( lifeInit
       ) where

import Path (mkRelFile)
import Path.IO (copyFile, doesDirExist, doesFileExist)

import Life.Configuration (LifeConfiguration (..), lifePath, renderLifeConfiguration,
                           singleFileConfig, writeGlobalLife)
import Life.Github (Owner, Repo (Repo), createRepository, doesRepoExist, insideRepo, repoName)
import Life.Message (chooseYesNo, errorMessage, infoMessage, skipMessage, successMessage,
                     warningMessage)
import Life.Shell (createDirInHome, relativeToHome)

import qualified Data.Set as Set

predefinedLifeConfig :: LifeConfiguration
predefinedLifeConfig = mempty
    { lifeConfigurationFiles = Set.fromList
          [ $(mkRelFile ".bash_profile")
          , $(mkRelFile ".profile")
          , $(mkRelFile ".vimrc")
          , $(mkRelFile ".emacs")
          , $(mkRelFile ".spacemacs")
          , $(mkRelFile ".gitconfig")
          , $(mkRelFile ".ghc/ghci.conf")
          , $(mkRelFile ".stylish-haskell.yaml")
          ]
    }

lifeInit :: Owner -> IO ()
lifeInit owner = do
    -- create initial life configuration
    lifeFilePath <- relativeToHome lifePath
    -- check for .life existence
    isFile <- doesFileExist lifeFilePath
    if isFile
        then do
            warningMessage ".life file is already exist."
            useIt <- chooseYesNo "Would you like to use it?"
            unless useIt createLifeFile
        else createLifeFile

    -- check for dotfiles existence
    whenM doesRepoExist $
        errorMessage "dotfiles folder already exist" >> exitFailure

    -- create dotfiles repository
    () <$ createDirInHome repoName
    insideRepo $ do
        -- TODO: use list of some predefined files and directories
        copyFile lifeFilePath lifePath
        createRepository owner (Repo "dotfiles")
  where
    createLifeFile :: IO ()
    createLifeFile = do
        infoMessage "Checking existence of some commonly used predefined files..."
        (exist, noExist) <- scanConfig predefinedLifeConfig

        unless (noExist == mempty) $ do
            infoMessage "The following files and directories weren't found (so just ignoring them):"
            skipMessage $ renderLifeConfiguration noExist

        unless (exist == mempty) $ do
            infoMessage "Found the following files and directories:"
            successMessage $ renderLifeConfiguration exist

        useDiscovered <- chooseYesNo "Would you like to add all discovered existing files and directories to .life configuration?"
        let lifeConfig = singleFileConfig lifePath <> (if useDiscovered then exist else mempty)

        infoMessage "Initializing global .life configuration file..."
        writeGlobalLife lifeConfig

{- | Split given configuration into two:

1. All files and directories which exist on machine.
2. Other non-existing files and dirs.
-}
scanConfig :: LifeConfiguration -> IO (LifeConfiguration, LifeConfiguration)
scanConfig LifeConfiguration{..} = do
    (existingFiles, nonExistingFiles) <- partitionM (relativeToHome >=> doesFileExist) lifeConfigurationFiles
    (existingDirs, nonExistingDirs) <- partitionM (relativeToHome >=> doesDirExist) lifeConfigurationDirectories
    pure ( LifeConfiguration (Set.fromList existingFiles) (Set.fromList existingDirs)
         , LifeConfiguration (Set.fromList nonExistingFiles) (Set.fromList nonExistingDirs)
         )

partitionM :: forall f m a . (Monad m, Foldable f) => (a -> m Bool) -> f a -> m ([a], [a])
partitionM check = foldM partitionAction ([], [])
  where
    partitionAction :: ([a], [a]) -> a -> m ([a], [a])
    partitionAction (ifTrue, ifFalse) a = check a >>= \case
        True  -> pure (a : ifTrue, ifFalse)
        False -> pure (ifTrue, a : ifFalse)
