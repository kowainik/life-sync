{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Main entry point for the @life@ executable.
-}

module Life.Main
    ( lifeMain
    ) where

import Path (parseRelDir, parseRelFile)

import Life.Cli (LifeCommand (..), PathOptions (..), PullOptions (..), parseCommand)
import Life.Main.Add (lifeAdd)
import Life.Main.Init (lifeInit)
import Life.Main.Pull (lifePull)
import Life.Main.Push (lifePush)
import Life.Main.Remove (lifeRemove)

import qualified Data.Set as Set


lifeMain :: IO ()
lifeMain = parseCommand >>= \case
    Init   owner           -> lifeInit   owner
    Add    PathOptions{..} -> lifeAdd    pathOptionsPath
    Remove PathOptions{..} -> lifeRemove pathOptionsPath
    Push                   -> lifePush
    Pull   PullOptions{..} -> do
        withoutFiles <- Set.fromList <$> mapM parseRelFile pullOptionsNoFiles
        withoutDirs  <- Set.fromList <$> mapM parseRelDir  pullOptionsNoDirs
        lifePull pullOptionsOwner withoutFiles withoutDirs
