{-# LANGUAGE Rank2Types #-}

-- | Functions to switch git branches.

module Life.Main.Switch
       ( lifeSwitch
       ) where

import Life.Core (Branch (..))
import Life.Configuration (LifeConfiguration (..), parseHomeLife, writeGlobalLife)
import Life.Github (createNewBranch, setCurrentBranch, isBranchExists)
import Life.Message (abortCmd)
import Life.Shell (LifeExistence (..), whatIsLife)


-- | Remove path from existing life-configuration file.
lifeSwitch :: Branch -> IO ()
lifeSwitch branch = whatIsLife >>= \case
    -- if one of them is missing -- abort
    NoLife -> abortCmd "remove" ".life and docfiles/ do not exist"
    OnlyLife _ -> abortCmd "remove" "dotfiles/ directory doesn't exist"
    OnlyRepo _ -> abortCmd "remove" ".life file doesn't exist"
    Both _ _   -> resolveConfiguration branch

resolveConfiguration :: Branch -> IO ()
resolveConfiguration branch = do
    configuration <- parseHomeLife
    writeGlobalLife configuration { lifeConfigurationBranch = Last (Just branch) }
    branchExists <- isBranchExists branch
    if branchExists
        then setCurrentBranch branch
        else createNewBranch branch
