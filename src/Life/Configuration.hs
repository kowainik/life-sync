-- | Contains configuration data type.

module Life.Configuration
       ( LifeConfiguration (..)
       ) where

import Universum

import Path (Dir, File, Path, Rel)

data LifeConfiguration = LifeConfiguration
     { lifeConfigurationFiles       :: [Path Rel File]
     , lifeConfigurationDirectories :: [Path Rel Dir]
     } deriving (Show)
