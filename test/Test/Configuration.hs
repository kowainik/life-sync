{-# LANGUAGE QuasiQuotes #-}

module Test.Configuration
    ( configurationSpec
    ) where

import Data.Foldable (foldr1)
import Hedgehog (Gen, PropertyT, forAll, tripping)
import Path (reldir, relfile)
import Path.Internal (Path (Path))
import Relude.Extra.Lens ((.~))
import System.FilePath (pathSeparator, (</>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Life.Configuration (LifeConfiguration (..), corpseConfiguationCodec, directoriesL, filesL,
                           renderLifeConfiguration, resurrect)
import Life.Core (master)

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Toml


configurationSpec :: Spec
configurationSpec = describe "Configuration tests" $ do
    configurationRenderSpec
    configurationPropertySpec

configurationRenderSpec :: Spec
configurationRenderSpec = describe "Pretty rendering" $ do
    it "empty configuration" $
        render mempty `shouldBe` emptyTxt
    it "non-empty configuration" $
        render nonEmptyCfg `shouldBe` nonEmptyTxt
  where
    render :: LifeConfiguration -> Text
    render = renderLifeConfiguration True

    emptyTxt :: Text
    emptyTxt = T.intercalate "\n"
        [ "directories = []"
        , "files = []"
        ]

    nonEmptyCfg :: LifeConfiguration
    nonEmptyCfg = mempty
        & directoriesL .~ one [reldir|foo/|]
        & filesL .~ Set.fromList
            [ [relfile|bar.txt|]
            , [relfile|quux|]
            ]

    nonEmptyTxt :: Text
    nonEmptyTxt = T.intercalate "\n"
        [ "directories = [\"foo/\"]"
        , "files = "
        , "    [ \"bar.txt\""
        , "    , \"quux\""
        , "    ]"
        ]


configurationPropertySpec :: Spec
configurationPropertySpec = describe "Property Tests" $
    it "parseLifeConfiguration . renderLifeConfiguration cfg ≡ Just cfg"
        parseRenderSpec

parseRenderSpec :: PropertyT IO ()
parseRenderSpec = hedgehog $ do
    cfg <- forAll genLifeConfiguration
    tripping cfg
        (renderLifeConfiguration True)
        (\t -> rightToMaybe (Toml.decode corpseConfiguationCodec t) >>= resurrect)

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
