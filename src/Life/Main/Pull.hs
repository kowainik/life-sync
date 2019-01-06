-- | Command to update local state from remote state.

module Life.Main.Pull
       ( lifePull
       ) where

import Path (Dir, File, Path, Rel)

import Life.Configuration (LifeConfiguration (..), defaultLifeConfig)
import Life.Github (cloneRepo, pullUpdateFromRepo, updateFromRepo)
import Life.Core (Owner, master)
import Life.Main.Init (lifeInitQuestion)
import Life.Message (abortCmd, choose, warningMessage)
import Life.Shell (LifeExistence (..), whatIsLife)

lifePull :: Owner -> Set (Path Rel File) -> Set (Path Rel Dir) -> IO ()
lifePull owner withoutFiles withoutDirs = whatIsLife >>= \case
    OnlyRepo _ -> warningMessage ".life file not found" >> pullUpdate
    OnlyLife _ -> warningMessage "dotfiles not found" >> clone >> update
    NoLife     -> initOrPull
    Both _ _   -> pullUpdate
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
    life = defaultLifeConfig { lifeConfigurationDirectories = withoutDirs
                             , lifeConfigurationFiles = withoutFiles
                             }

    clone, update, pullUpdate :: IO ()
    clone = cloneRepo owner
    update = updateFromRepo life
    pullUpdate = pullUpdateFromRepo life
