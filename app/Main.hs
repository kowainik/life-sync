module Main where

import Life.Main.Init (lifeInit)
import Life.Main.Remove (lifeRemove)
import Life.Main.Update (lifeAdd)

import Options (FileOptions (..), InitOptions (..), LifeCommand (..), parseCommand)

main :: IO ()
main = parseCommand >>= \case
    Init   InitOptions{..}  -> lifeInit   initOptionsOwner
    Add    FileOptions{..}  -> lifeAdd    fileOptionsFile
    Remove FileOptions{..}  -> lifeRemove fileOptionsFile
