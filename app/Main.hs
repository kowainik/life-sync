module Main where

import Universum

import Life.Configuration (LifeConfiguration (..))

import Options (LifeCommand (..), parseCommand)

main :: IO ()
main = parseCommand >>= \case
    Init _ -> putText "Hello!"
    _      -> putText "Bye!!!"
