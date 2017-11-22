{-# LANGUAGE TemplateHaskell #-}
-- | Utilities to work with GitHub repositories using "hub".

module Life.Github
       ( Owner (..)
       , Repo  (..)

       , createRepository
       , repoName
       ) where

import Universum

import Path (Dir, Path, Rel, mkRelDir)

import Life.Shell ()

newtype Owner = Owner { getOwner :: Text } deriving (Show)
newtype Repo  = Repo  { getRepo  :: Text } deriving (Show)

-- | Default repository name for life configuration files.
repoName :: Path Rel Dir
repoName = $(mkRelDir "dotfiles/")

-- | Make a commit and push it.
pushka :: IO ()
pushka = do
    "git" ["add", "."]
    "git" ["commit", "-m", "Create the project"]
    "git" ["push", "-u", "origin", "master"]

-- | Creates repository on GitHub inside given folder.
createRepository :: Owner -> Repo -> IO ()
createRepository (Owner owner) (Repo repo) = do
    let description = ":computer: Configuration files"
    "git" ["init"]
    "hub" ["create", "-d", description, owner <> "/" <> repo]
    pushka
