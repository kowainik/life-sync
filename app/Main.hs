module Main where

import Life.Main.Add (lifeAdd)
import Life.Main.Init (lifeInit)
import Life.Main.Remove (lifeRemove)

import Options (FileOptions (..), InitOptions (..), LifeCommand (..), parseCommand)

main :: IO ()
main = parseCommand >>= \case
    Init   InitOptions{..}  -> lifeInit   initOptionsOwner
    Add    FileOptions{..}  -> lifeAdd    fileOptionsFile
    Remove FileOptions{..}  -> lifeRemove fileOptionsFile
