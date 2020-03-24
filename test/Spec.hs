module Main (main) where

import Test.Hspec (hspec)

import Test.Configuration (configurationSpec)

main :: IO ()
main = hspec
    configurationSpec
