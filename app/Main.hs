module Main where

import Universum

import Life.Main.Init (initLife)

import Options (InitOptions (..), LifeCommand (..), parseCommand)

main :: IO ()
main = parseCommand >>= \case
    Init InitOptions{..} -> initLife initOptionsOwner
