{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Contains configuration data type.

module Life.Configuration
       ( LifeConfiguration  (..)
       , singleDirConfig
       , singleFileConfig

--         -- * Parsing exceptions
--       , ParseLifeException (..)

         -- * Path to life
       , lifePath

         -- * Lenses for 'LifeConfiguration'
       , files
       , directories

         -- * Parse and render 'LifeConfiguration' under `~/.life`
       , parseGlobalLife
       , writeGlobalLife
       ) where

import Control.Exception.Base (throwIO)
import Fmt (indentF, unlinesF, (+|), (|+))
import Lens.Micro.Platform (makeFields)
import Path (Dir, File, Path, Rel, fromAbsFile, mkRelFile, parseRelDir, parseRelFile, toFilePath)
import Toml (BiToml, Valuer (..), (.=))

import Life.Shell (relativeToHome)

import qualified Data.Set as Set
import qualified Text.Show as Show
import qualified Toml

----------------------------------------------------------------------------
-- Life Configuration data type with lenses
----------------------------------------------------------------------------

data LifeConfiguration = LifeConfiguration
     { lifeConfigurationFiles       :: Set (Path Rel File)
     , lifeConfigurationDirectories :: Set (Path Rel Dir)
     } deriving (Show)

-- | Name for life configuration file.
lifePath :: Path Rel File
lifePath = $(mkRelFile ".life")

makeFields ''LifeConfiguration

----------------------------------------------------------------------------
-- Algebraic instances and utilities
----------------------------------------------------------------------------

instance Semigroup LifeConfiguration where
    life1 <> life2 = LifeConfiguration
        { lifeConfigurationFiles       = life1^.files <> life2^.files
        , lifeConfigurationDirectories = life1^.directories <> life2^.directories
        }

instance Monoid LifeConfiguration where
    mempty  = LifeConfiguration mempty mempty
    mappend = (<>)

singleFileConfig :: Path Rel File -> LifeConfiguration
singleFileConfig file = mempty & files .~ one file

singleDirConfig :: Path Rel Dir -> LifeConfiguration
singleDirConfig dir = mempty & directories .~ one dir

----------------------------------------------------------------------------
-- Toml parser for life configuration
----------------------------------------------------------------------------

data CorpseConfiguration = CorpseConfiguration
    { corpseFiles       :: [FilePath]
    , corpseDirectories :: [FilePath]
    }

corpseConfiguationT :: BiToml CorpseConfiguration
corpseConfiguationT = CorpseConfiguration
    <$> Toml.arrayOf stringV "files"       .= corpseFiles
    <*> Toml.arrayOf stringV "directories" .= corpseDirectories
  where
    stringV :: Valuer 'Toml.TString String
    stringV = Valuer (Toml.matchText >=> pure . toString) (Toml.String . toText)

resurrect :: CorpseConfiguration -> IO LifeConfiguration
resurrect CorpseConfiguration{..} = do
    filePaths <- mapM parseRelFile corpseFiles
    dirPaths  <- mapM parseRelDir  corpseDirectories

    pure $ LifeConfiguration
        { lifeConfigurationFiles = Set.fromList filePaths
        , lifeConfigurationDirectories = Set.fromList dirPaths
        }

-- TODO: should tomland one day support this?...
-- | Converts 'LifeConfiguration' into TOML file.
renderLifeConfiguration :: LifeConfiguration -> Text
renderLifeConfiguration LifeConfiguration{..} = mconcat
    [ render "files      " lifeConfigurationFiles
    , "\n"
    , render "directories" lifeConfigurationDirectories
    ]
  where
    render :: Text -> Set (Path b t) -> Text
    render key paths = do
        let prefix = key <> " = "
        let array  = renderStringArray (length prefix) (map show $ toList paths)
        prefix <> array

    renderStringArray :: Int -> [String] -> Text
    renderStringArray _ []     = "[]"
    renderStringArray n (x:xs) = "[ " +| x |+ "\n"
                              +| indentF n (unlinesF (map (", " ++) xs ++ ["]"]))
                              |+ ""

writeGlobalLife :: LifeConfiguration -> IO ()
writeGlobalLife config = do
    lifeFilePath <- relativeToHome lifePath
    writeFile (fromAbsFile lifeFilePath) (renderLifeConfiguration config)

----------------------------------------------------------------------------
-- Life configuration parsing
----------------------------------------------------------------------------

-- | Reads 'LifeConfiguration' from @~\/.life@ file.
parseGlobalLife :: IO LifeConfiguration
parseGlobalLife = do
    lifeFilePath <- relativeToHome lifePath
    tomlText     <- readFile $ fromAbsFile lifeFilePath
    case Toml.decode corpseConfiguationT tomlText of
        Left err -> throwIO $ LoadTomlException (toFilePath lifeFilePath) $ Toml.prettyException err
        Right cfg -> resurrect cfg

data LoadTomlException = LoadTomlException FilePath Text

instance Show.Show LoadTomlException where
    show (LoadTomlException filePath msg) = "Couldnt parse file " ++ filePath ++ ": " ++ show msg

instance Exception LoadTomlException
