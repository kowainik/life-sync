module Main where

import Life.Main.Add (lifeAdd)
import Life.Main.Init (lifeInit)
import Life.Main.Push (lifePush)
import Life.Main.Remove (lifeRemove)

import Options (InitOptions (..), LifeCommand (..), PathOptions (..), parseCommand)

main :: IO ()
main = parseCommand >>= \case
    Init   InitOptions{..}  -> lifeInit   initOptionsOwner
    Add    PathOptions{..}  -> lifeAdd    pathOptionsPath
    Remove PathOptions{..}  -> lifeRemove pathOptionsPath
    Push                    -> lifePush
