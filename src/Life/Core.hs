{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains main data types used in the project.
-}

module Life.Core
    (
      -- * Git and Github core
      Branch (..)
    , Owner  (..)
    , Repo   (..)
    , CommitMsg (..)
    , master

      -- * File system logic
    , CopyDirection (..)
    , LifePath (..)
    ) where

----------------------------------------------------------------------------
-- Git and Github core
----------------------------------------------------------------------------

-- | Github repository owner.
newtype Owner = Owner
    { unOwner :: Text
    } deriving stock (Show)

-- | Git repository.
newtype Repo  = Repo
    { unRepo :: Text
    } deriving stock (Show)

-- | Git branch.
newtype Branch = Branch
    { unBranch :: Text
    } deriving stock (Show)
      deriving newtype (Eq)

-- | Git commit message.
newtype CommitMsg = CommitMsg
    { unCommitMsg :: Text
    } deriving stock (Show)

-- | Git "master" branch constant.
master :: Branch
master = Branch "master"

----------------------------------------------------------------------------
-- File system logic
----------------------------------------------------------------------------

data CopyDirection
    = FromHomeToRepo
    | FromRepoToHome

data LifePath
    = File FilePath
    | Dir FilePath
    deriving stock (Show)
