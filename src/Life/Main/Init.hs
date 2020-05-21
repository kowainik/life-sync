{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains a function to create the main repository — @dotfiles@.
-}

module Life.Main.Init
    ( lifeInit
    , lifeInitQuestion
    ) where

import Colourista (infoMessage, skipMessage, successMessage, warningMessage)
import Path (mkRelFile)
import Path.IO (doesDirExist, doesFileExist)


import Life.Configuration (LifeConfiguration (..), parseHomeLife, renderLifeConfiguration,
                           singleFileConfig, writeGlobalLife)
import Life.Core (CopyDirection (..), Owner (..), Repo (..), master)
import Life.Github (copyLife, createRepository, insideRepo)
import Life.Message (abortCmd, chooseYesNo, promptNonEmpty)
import Life.Path (LifeExistence (..), createDirInHome, lifePath, relativeToHome, repoName,
                  whatIsLife)

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

lifeInit :: Maybe Owner -> IO ()
lifeInit owner = whatIsLife >>= \case
    NoLife -> createLifeFile >>= createDotfilesDir
    OnlyLife _ -> askCreateLife >>= createDotfilesDir
    OnlyRepo _ -> abortCmd "init" "'~/dotfiles' directory already exist"  -- TODO: initialize .life from repo? :thinking_suicide:
    Both _ _ -> abortCmd "init" "'~/.life' file and '~/.dotfiles' directory are already initialized"
  where
    askCreateLife :: IO LifeConfiguration
    askCreateLife = do
        warningMessage ".life file is already exist."
        useIt <- chooseYesNo "Would you like to use it?"
        if useIt then parseHomeLife else createLifeFile

    createLifeFile :: IO LifeConfiguration
    createLifeFile = do
        infoMessage "Checking existence of some commonly used predefined files..."
        (exist, noExist) <- scanConfig predefinedLifeConfig

        unless (noExist == mempty) $ do
            infoMessage "The following files and directories weren't found; they won't be added to '~/.life' file:"
            skipMessage $ renderLifeConfiguration False noExist

        unless (exist == mempty) $ do
            infoMessage "Found the following files and directories:"
            successMessage $ renderLifeConfiguration False exist

        useDiscovered <- chooseYesNo "Would you like to add all discovered existing files and directories to .life configuration?"
        let lifeConfig = singleFileConfig lifePath <> (if useDiscovered then exist else mempty)

        infoMessage "Initializing global .life configuration file..."
        writeGlobalLife lifeConfig
        pure lifeConfig

    createDotfilesDir :: LifeConfiguration -> IO ()
    createDotfilesDir lifeConfig = do
        () <$ createDirInHome repoName
        insideRepo $ do
            copyLife FromHomeToRepo lifeConfig
            createRepository owner (Repo "dotfiles")

{- | Split given configuration into two:

1. All files and directories which exist on machine.
2. Other non-existing files and dirs.
-}
scanConfig :: LifeConfiguration -> IO (LifeConfiguration, LifeConfiguration)
scanConfig LifeConfiguration{..} = do
    (existingFiles, nonExistingFiles) <- partitionM (relativeToHome >=> doesFileExist) lifeConfigurationFiles
    (existingDirs, nonExistingDirs) <- partitionM (relativeToHome >=> doesDirExist) lifeConfigurationDirectories
    pure ( LifeConfiguration (Set.fromList existingFiles) (Set.fromList existingDirs) (Last $ Just master)
         , LifeConfiguration (Set.fromList nonExistingFiles) (Set.fromList nonExistingDirs) (Last $ Just master)
         )

partitionM :: forall f m a . (Monad m, Foldable f) => (a -> m Bool) -> f a -> m ([a], [a])
partitionM check = foldlM partitionAction ([], [])
  where
    partitionAction :: ([a], [a]) -> a -> m ([a], [a])
    partitionAction (ifTrue, ifFalse) a = check a >>= \case
        True  -> pure (a : ifTrue, ifFalse)
        False -> pure (ifTrue, a : ifFalse)


-- | If @.life@ and @dotfiles@ are not present you could want
-- to ask one if it needed to be initialised.
lifeInitQuestion :: Text  -- ^ Command name
                 -> IO () -- ^ Process to do
                 -> IO ()
lifeInitQuestion cmd process = do
    warningMessage ".life file and dotfiles/ do not exist"
    toInit <- chooseYesNo "Would you like to proceed initialization process?"
    if toInit then do
        infoMessage "Initialization process starts.."
        skipMessage "Insert your GitHub username:"
        owner <- promptNonEmpty
        lifeInit $ Just $ Owner owner
        process
    else abortCmd cmd "'~/.life' file is not initialized"
