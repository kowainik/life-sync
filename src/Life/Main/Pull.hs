-- | Command to update local state from remote state.

module Life.Main.Pull
       ( lifePull
       ) where

import Path (Dir, File, Path, Rel)

import Life.Configuration (LifeConfiguration (..), getBranch, parseHomeLife)
import Life.Github (Owner, cloneRepo, pullUpdateFromRepo, master, updateFromRepo, setCurrentBranch)
import Life.Main.Init (lifeInitQuestion)
import Life.Message (abortCmd, choose, warningMessage)
import Life.Shell (LifeExistence (..), whatIsLife)

lifePull :: Owner -> Set (Path Rel File) -> Set (Path Rel Dir) -> IO ()
lifePull owner withoutFiles withoutDirs = do
    homeLife <- parseHomeLife
    let branch = getBranch homeLife
    whatIsLife >>= \case
            OnlyRepo _ -> warningMessage ".life file not found" >> setCurrentBranch branch >> pullUpdate
            OnlyLife _ -> warningMessage "dotfiles not found" >> clone >> setCurrentBranch branch >> update
            NoLife     -> initOrPull >> setCurrentBranch branch
            Both _ _   -> setCurrentBranch branch >> pullUpdate
  where
    initOrPull :: IO ()
    initOrPull = do
        warningMessage ".life file and dotfiles repo not found"
        action <- choose "Do you want to (F)etch existing repo, (I)nit from scratch or (A)bort operation?"
                         ["f", "i", "a"]
        case action of
            "f" -> clone >> update
            "i" -> lifeInitQuestion "pull" pass
            "a" -> abortCmd "pull" "Cannot find .life and dotfiles"
            _   -> error "Impossible choice"

    life :: LifeConfiguration
    life = LifeConfiguration withoutFiles withoutDirs (Last $ Just master)

    clone, update, pullUpdate :: IO ()
    clone = cloneRepo owner
    update     = updateFromRepo life
    pullUpdate = pullUpdateFromRepo life
