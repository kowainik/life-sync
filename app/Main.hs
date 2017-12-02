module Main where

import Universum

import Life.Main.Init (lifeInit)
import Life.Main.Update (lifeAdd)

import Options (AddOptions (..), InitOptions (..), LifeCommand (..), parseCommand)

main :: IO ()
main = parseCommand >>= \case
    Init InitOptions{..} -> lifeInit initOptionsOwner
    Add  AddOptions{..}  -> lifeAdd  addOptionsFile
