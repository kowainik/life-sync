{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Utilities to work with GitHub repositories using @hub@.
-}

module Life.Github
    (
      -- * Repository utils
      checkRemoteSync
    , cloneRepo
    , createNewBranch
    , doesBranchExist
    , insideRepo
    , withSynced

      -- * Repository manipulation commands
    , copyLife
    , addToRepo
    , createRepository
    , pullUpdateFromRepo
    , removeFromRepo
    , updateDotfilesRepo
    , updateFromRepo
    , getUserLogin
    ) where

import Colourista (errorMessage, infoMessage, warningMessage)
import Control.Exception (catch, throwIO)
import Path (Abs, Dir, File, Path, Rel, toFilePath, (</>))
import Path.IO (copyDirRecur, copyFile, getHomeDir, withCurrentDir)
import Shellmet (($|))
import System.IO.Error (IOError, isDoesNotExistError)

import Life.Configuration (LifeConfiguration (..), lifeConfigMinus, parseRepoLife)
import Life.Core (Branch (..), CommitMsg (..), CopyDirection (..), Owner (..), Repo (..), master)
import Life.Message (chooseYesNo)
import Life.Path (lifePath, relativeToHome, repoName)

import qualified Data.Text as Text


----------------------------------------------------------------------------
-- VSC commands
----------------------------------------------------------------------------

askToPushka :: CommitMsg -> IO ()
askToPushka commitMsg = do
    "git" ["add", "."]
    infoMessage "The following changes are going to be pushed:"
    "git" ["diff", "--name-status", "HEAD"]
    continue <- chooseYesNo "Would you like to proceed?"
    if continue
    then pushka master commitMsg
    else errorMessage "Abort pushing" >> exitFailure

-- | Make a commit and push it.
pushka :: Branch -> CommitMsg -> IO ()
pushka (Branch branch) (CommitMsg commitMsg) = do
    "git" ["add", "."]
    "git" ["commit", "-m", commitMsg]
    "git" ["push", "-u", "origin", branch]

-- | Creates repository on GitHub inside given folder.
createRepository :: Maybe Owner -> Repo -> IO ()
createRepository mo (Repo repo) = do
    owner <- getOwnerLogin mo
    let description = ":computer: Configuration files"
    "git" ["init"]
    "hub" ["create", "-d", description, owner <> "/" <> repo]
    pushka master $ CommitMsg "Create the project"

-- | Get user login from the local global git config.
getUserLogin :: IO Text
getUserLogin = do
    login <- "git" $| ["config", "user.login"]
    if login == ""
        then errorMessage "user.login is not specified" >> exitFailure
        else pure $ toText login

-- | Consider owner from global git config if Owner is not given
getOwnerLogin :: Maybe Owner -> IO Text
getOwnerLogin = maybe getUserLogin (pure . unOwner)

----------------------------------------------------------------------------
-- dotfiles workflow
----------------------------------------------------------------------------

-- | Executes action with 'repoName' set as pwd.
insideRepo :: IO a -> IO a
insideRepo action = do
    repoPath <- relativeToHome repoName
    withCurrentDir repoPath action

-- | Commits all changes inside 'repoName' and pushes to remote.
pushRepo :: CommitMsg -> IO ()
pushRepo = insideRepo . askToPushka

-- | Clones @dotfiles@ repository assuming it doesn't exist.
cloneRepo :: Maybe Owner -> IO ()
cloneRepo mo = do
    owner <- getOwnerLogin mo
    homeDir <- getHomeDir
    withCurrentDir homeDir $ do
        infoMessage "Using SSH to clone repo..."
        "git" ["clone", "git@github.com:" <> owner <> "/dotfiles.git"]

-- | Create new branch with given branch name
createNewBranch :: Branch -> IO ()
createNewBranch (Branch branch) =
    "git" ["checkout", "-b", branch]

-- | Returns true if local @dotfiles@ repository is synchronized with remote repo.
checkRemoteSync :: Branch -> IO Bool
checkRemoteSync (Branch branch) = do
    "git" ["fetch", "origin", branch]
    localHash  <- "git" $| ["rev-parse", branch]
    remoteHash <- "git" $| ["rev-parse", "origin/" <> branch]
    pure $ localHash == remoteHash

-- | Check if a branch exists in remote repo
doesBranchExist :: Branch -> IO Bool
doesBranchExist (Branch branch) = do
    r <- "git" $| ["ls-remote", "--heads", "origin", branch]
    pure $ not (Text.null r)

withSynced :: Branch -> IO a -> IO a
withSynced branch@(Branch branchname) action = insideRepo $ do
    infoMessage "Checking if repo is synchronized..."
    isSynced <- checkRemoteSync branch
    if isSynced then do
        infoMessage "Repo is up-to-date"
        action
    else do
        warningMessage "Local version of repository is out of date"
        shouldSync <- chooseYesNo "Do you want to sync repo with remote?"
        if shouldSync then do
            "git" ["rebase", "origin/" <> branchname]
            action
        else do
            errorMessage "Aborting current command because repository is not synchronized with remote"
            exitFailure

----------------------------------------------------------------------------
-- File manipulation
----------------------------------------------------------------------------

pullUpdateFromRepo :: LifeConfiguration -> IO ()
pullUpdateFromRepo life = do
    insideRepo $ "git" ["pull", "-r"]
    updateFromRepo life

updateFromRepo :: LifeConfiguration -> IO ()
updateFromRepo excludeLife = insideRepo $ do
    infoMessage "Copying files from repo to local machine..."

    repoLife <- parseRepoLife
    let lifeToLive = lifeConfigMinus repoLife excludeLife

    copyLife FromRepoToHome lifeToLive

updateDotfilesRepo :: CommitMsg -> LifeConfiguration -> IO ()
updateDotfilesRepo commitMsg life = do
    copyLife FromHomeToRepo life
    pushRepo commitMsg

copyLife :: CopyDirection -> LifeConfiguration -> IO ()
copyLife direction LifeConfiguration{..} = do
    copyFiles direction (toList lifeConfigurationFiles)
    copyDirs  direction (toList lifeConfigurationDirectories)

-- | Copy files to repository and push changes to remote repository.
copyFiles :: CopyDirection -> [Path Rel File] -> IO ()
copyFiles = copyPathList copyFile

-- | Copy dirs to repository.
copyDirs :: CopyDirection -> [Path Rel Dir] -> IO ()
copyDirs = copyPathList copyDirRecur

copyPathList
    :: (Path Abs t -> Path Abs t -> IO ())
    -- ^ Copying action
    -> CopyDirection
    -- ^ Describes in which direction files should be copied
    -> [Path Rel t]
    -- ^ List of paths to copy
    -> IO ()
copyPathList copyAction direction pathList = do
    homeDir    <- getHomeDir
    let repoDir = homeDir </> repoName

    for_ pathList $ \entryPath -> do
        let homePath = homeDir </> entryPath
        let repoPath = repoDir </> entryPath
        case direction of
            FromHomeToRepo -> copyAction homePath repoPath
            FromRepoToHome -> copyAction repoPath homePath

-- | Update .life file
updateLifeFile :: IO ()
updateLifeFile = do
    lifeFile <- relativeToHome lifePath
    repoLifeFile <- relativeToHome (repoName </> lifePath)
    copyFile lifeFile repoLifeFile

-- | Adds file or directory to the repository and commits
addToRepo :: (Path Abs t -> Path Abs t -> IO ()) -> Path Rel t -> IO ()
addToRepo copyFun path = do
    -- copy file
    sourcePath <- relativeToHome path
    destinationPath <- relativeToHome (repoName </> path)
    copyFun sourcePath destinationPath

    updateLifeFile

    let commitMsg = CommitMsg $ "Add: " <> toText (toFilePath path)
    pushRepo commitMsg

-- | Removes file or directory from the repository and commits
removeFromRepo :: (Path Abs t -> IO ()) -> Path Rel t -> IO ()
removeFromRepo removeFun path = do
    absPath <- relativeToHome (repoName </> path)
    catch (removeFun absPath) handleNotExist

    updateLifeFile

    let commitMsg = CommitMsg $ "Remove: " <> pathTextName
    pushRepo commitMsg
  where
    pathTextName :: Text
    pathTextName = toText $ toFilePath path

    handleNotExist :: IOError -> IO ()
    handleNotExist e = if isDoesNotExistError e
        then errorMessage ("File/directory " <> pathTextName <> " is not found") >> exitFailure
        else throwIO e
