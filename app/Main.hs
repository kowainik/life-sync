module Main where

import Path (parseRelDir, parseRelFile)

import Life.Main.Add (lifeAdd)
import Life.Main.Init (lifeInit)
import Life.Main.Pull (lifePull)
import Life.Main.Push (lifePush)
import Life.Main.Remove (lifeRemove)

import Options (LifeCommand (..), PathOptions (..), PullOptions (..), parseCommand)

import qualified Data.Set as Set

main :: IO ()
main = parseCommand >>= \case
    Init   owner           -> lifeInit   owner
    Add    PathOptions{..} -> lifeAdd    pathOptionsPath
    Remove PathOptions{..} -> lifeRemove pathOptionsPath
    Push                   -> lifePush
    Pull   PullOptions{..} -> do
        withoutFiles <- Set.fromList <$> mapM parseRelFile pullOptionsNoFiles
        withoutDirs  <- Set.fromList <$> mapM parseRelDir  pullOptionsNoDirs
        lifePull pullOptionsOwner withoutFiles withoutDirs
