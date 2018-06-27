-- | Command to update local state from remote state.

module Life.Main.Pull
       ( lifePull
       ) where

import Path (Dir, File, Path, Rel)

import Life.Github (Owner, cloneRepo, updateFromRepo)
import Life.Main.Init (lifeInitQuestion)
import Life.Message (abortCmd, choose, warningMessage)
import Life.Shell (LifeExistence (..), whatIsLife)

lifePull :: Owner -> Set (Path Rel File) -> Set (Path Rel Dir) -> IO ()
lifePull owner withoutFiles withoutDirs = whatIsLife >>= \case
    OnlyRepo _ -> warningMessage ".life file not found" >> update
    OnlyLife _ -> warningMessage "dotfiles not found" >> clone >> update
    NoLife     -> initOrPull
    Both _ _   -> update
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

    clone, update :: IO ()
    clone  = cloneRepo owner
    update = updateFromRepo withoutFiles withoutDirs
