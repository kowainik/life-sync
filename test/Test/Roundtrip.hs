module Test.Roundtrip
    ( hprop_ConfigurationRoundtrip
    ) where

import Data.Foldable (foldr1)
import Hedgehog (Gen, Property, forAll, property, tripping)
import Path.Internal (Path (Path))
import System.FilePath (pathSeparator, (</>))

import Life.Configuration (LifeConfiguration (..), parseLifeConfiguration, renderLifeConfiguration)
import Life.Core (master)

import qualified Data.Set as Set
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

hprop_ConfigurationRoundtrip :: Property
hprop_ConfigurationRoundtrip = property $ do
    cfg <- forAll genLifeConfiguration
    tripping cfg (renderLifeConfiguration True) (parseLifeConfiguration @Maybe)

genLifeConfiguration :: Gen LifeConfiguration
genLifeConfiguration = do
    lifeConfigurationFiles       <- genPathSet genFilePath
    lifeConfigurationDirectories <- genPathSet genDirPath
    let lifeConfigurationBranch = Last $ Just master
    pure LifeConfiguration{..}

-- it's safe to use 'Path' constructor here even if such things are not recommended by API
-- our generators should be safe; and if not - this will be caught by test later
genPathSet :: Gen FilePath -> Gen (Set (Path b t))
genPathSet gen = Set.fromList . fmap Path <$> Gen.list (Range.constant 0 30) gen

genDirPath :: Gen FilePath
genDirPath = (++ [pathSeparator]) <$> genFilePath

genFilePath :: Gen FilePath
genFilePath = foldr1 (</>) <$> Gen.nonEmpty (Range.constant 1 10) genFilePathPiece

genFilePathPiece :: Gen String
genFilePathPiece = Gen.string (Range.constant 1 10) Gen.alphaNum
