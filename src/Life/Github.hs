-- | Utilities to work with GitHub repositories using "hub".

module Life.Github
       ( Owner  (..)
       , Repo   (..)

         -- * Repository utils
       , checkRemoteSync
       , cloneRepo
       , insideRepo
       , isBranchExists
       , withSynced

         -- * Repository manipulation commands
       , addToRepo
       , copyLife
       , createNewBranch
       , createRepository
       , pullUpdateFromRepo
       , removeFromRepo
       , setCurrentBranch
       , updateDotfilesRepo
       , updateFromRepo

         -- * Constants
       , master
       ) where

import Control.Exception (catch, throwIO)
import Path (Abs, Dir, File, Path, Rel, toFilePath, (</>))
import Path.IO (copyDirRecur, copyFile, getHomeDir, withCurrentDir)
import System.IO.Error (IOError, isDoesNotExistError)

import Life.Core (Owner(..), Repo(..), Branch (..), CopyDirection (..), CommitMsg (..))
import Life.Configuration (LifeConfiguration (..), getBranch, lifeConfigMinus, parseRepoLife, parseHomeLife)
import Life.Message (chooseYesNo, errorMessage, infoMessage, warningMessage)
import Life.Shell (lifePath, relativeToHome, repoName, ($|))

import qualified Data.Text as T

----------------------------------------------------------------------------
-- VSC commands
----------------------------------------------------------------------------

askToPushka :: Branch -> CommitMsg -> IO ()
askToPushka branch@(Branch branchName) commitMsg = do
    "git" ["checkout", branchName]
    "git" ["add", "."]
    infoMessage "The following changes are going to be pushed:"
    "git" ["diff", "--name-status", "HEAD"]
    continue <- chooseYesNo "Would you like to proceed?"
    if continue
    then pushka commitMsg branch
    else errorMessage "Abort pushing" >> exitFailure

-- | Make a commit and push it.
pushka :: CommitMsg -> Branch -> IO ()
pushka (CommitMsg commitMsg) (Branch branch) = do
    "git" ["add", "."]
    "git" ["commit", "-m", commitMsg]
    "git" ["push", "-u", "origin", branch]

-- | Creates repository on GitHub inside given folder.
createRepository :: Owner -> Repo -> IO ()
createRepository (Owner owner) (Repo repo) = do
    let description = ":computer: Configuration files"
    "git" ["init"]
    "hub" ["create", "-d", description, owner <> "/" <> repo]
    pushka "Create the project" master

----------------------------------------------------------------------------
-- dotfiles workflow
----------------------------------------------------------------------------

-- | Executes action with 'repoName' set as pwd.
insideRepo :: IO a -> IO a
insideRepo action = do
    repoPath <- relativeToHome repoName
    withCurrentDir repoPath action

-- | Commits all changes inside 'repoName' and pushes to remote.
pushRepo :: CommitMsg -> Branch -> IO ()
pushRepo commitMsg branch = insideRepo $ askToPushka branch commitMsg

-- | Clones @dotfiles@ repository assuming it doesn't exist.
cloneRepo :: Owner -> IO ()
cloneRepo (Owner owner) = do
    homeDir <- getHomeDir
    withCurrentDir homeDir $ do
        infoMessage "Using SSH to clone repo..."
        "git" ["clone", "git@github.com:" <> owner <> "/dotfiles.git"]

-- | Returns true if local @dotfiles@ repository is synchronized with remote repo.
checkRemoteSync :: Branch -> IO Bool
checkRemoteSync (Branch branchName) = do
    "git" ["fetch", "origin"]
    localHash  <- "git" $| ["rev-parse", branchName]
    remoteHash <- "git" $| ["rev-parse", "origin/" <> branchName]
    pure $ localHash == remoteHash

withSynced :: Branch -> IO a -> IO a
withSynced branch@(Branch branchName) action = insideRepo $ do
    infoMessage "Checking if repo is synchnorized..."
    isSyсnced <- checkRemoteSync branch
    if isSyсnced then do
        infoMessage "Repo is up-to-date"
        action
        else do
            warningMessage "Local version of repository is out of date"
            shouldSync <- chooseYesNo "Do you want to sync repo with remote?"
            if shouldSync then do
                setCurrentBranch branch
                "git" ["rebase", "origin/" <> branchName]
                action
              else do
                errorMessage "Aborting current command because repository is not synchronized with remote"
                exitFailure

-- | Try to set branch in @dotfiles@
setCurrentBranch :: Branch  -> IO ()
setCurrentBranch (Branch branchName) = insideRepo $ "git" ["checkout", branchName]

-- | Check if branch exists
isBranchExists :: Branch -> IO Bool
isBranchExists (Branch branchName) = insideRepo $ do
    "git" ["fetch", "origin"]
    localBranches  <- "git" $| ["branch", "--list"]
    remoteBranches <- "git" $| ["branch", "--list", "-r"]
    pure $ elem branchName $ preformat localBranches ++ map cropRemotePrefixes (preformat remoteBranches)
    where preformat = T.lines . T.filter (not . (\el -> el `elem` ['*', ' '])) . T.pack
          cropRemotePrefixes raw = fromMaybe "" $ viaNonEmpty last $ T.splitOn "/" raw

-- | Create new branch, switch to it and push it to Github
createNewBranch :: Branch -> IO ()
createNewBranch (Branch branchName) = insideRepo $ do
    "git" ["checkout", "-b", branchName]
    "git" ["push", "--set-upstream", "origin", branchName]

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
    pushRepo commitMsg (getBranch life)

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

copyPathList :: (Path Abs t -> Path Abs t -> IO ())
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

-- | Adds file or directory to the repository and commits
addToRepo :: (Path Abs t -> Path Abs t -> IO ()) -> Path Rel t -> IO ()
addToRepo copyFun path = do
    life <- parseHomeLife

    -- copy file
    sourcePath <- relativeToHome path
    destinationPath <- relativeToHome (repoName </> path)
    copyFun sourcePath destinationPath

    -- update .life file
    lifeFile <- relativeToHome lifePath
    repoLifeFile <- relativeToHome (repoName </> lifePath)
    copyFile lifeFile repoLifeFile

    let commitMsg = CommitMsg $ "Add: " <> toText (toFilePath path)
    pushRepo commitMsg (getBranch life)

-- | Removes file or directory from the repository and commits
removeFromRepo :: (Path Abs t -> IO ()) -> Path Rel t -> IO ()
removeFromRepo removeFun path = do
    life <- parseHomeLife
    absPath <- relativeToHome (repoName </> path)
    catch (removeFun absPath) handleNotExist

    -- update .life file
    lifeFile <- relativeToHome lifePath
    repoLifeFile <- relativeToHome (repoName </> lifePath)
    copyFile lifeFile repoLifeFile

    let commitMsg = CommitMsg $ "Remove: " <> pathTextName
    pushRepo commitMsg (getBranch life)
  where
    pathTextName :: Text
    pathTextName = toText $ toFilePath path

    handleNotExist :: IOError -> IO ()
    handleNotExist e = if isDoesNotExistError e
        then errorMessage ("File/directory " <> pathTextName <> " is not found") >> exitFailure
        else throwIO e

-- | Git "master" branch constant.
master :: Branch
master = Branch "master"
