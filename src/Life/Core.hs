module Life.Core
    (
    -- * Git and Github core
      Branch (..)
    , Owner  (..)
    , Repo   (..)
    , CommitMsg (..)

    -- * File system logic
    , CopyDirection (..)
    , LifePath (..)
    ) where

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Git and Github core
----------------------------------------------------------------------------

-- | Github repository owner.
newtype Owner = Owner { unOwner  :: Text } deriving (Show)

-- | Git repository.
newtype Repo  = Repo  { unRepo   :: Text } deriving (Show)

-- | Git branch.
newtype Branch = Branch { unBranch :: Text } deriving (Eq, Show)

-- | Git commit message.
newtype CommitMsg = CommitMsg { unCommitMsg :: Text } deriving (Show)

instance IsString CommitMsg where
    fromString = CommitMsg . T.pack

----------------------------------------------------------------------------
-- File system logic
----------------------------------------------------------------------------

data CopyDirection = FromHomeToRepo | FromRepoToHome

data LifePath = File FilePath | Dir FilePath deriving (Show)
