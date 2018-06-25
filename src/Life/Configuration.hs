{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Contains configuration data type.

module Life.Configuration
       ( LifePath (..)

       , LifeConfiguration  (..)
       , singleDirConfig
       , singleFileConfig

       , lifeConfigMinus

--         -- * Parsing exceptions
--       , ParseLifeException (..)

         -- * Lenses for 'LifeConfiguration'
       , files
       , directories

         -- * Parse 'LifeConfiguration' under @~/.life@
       , parseGlobalLife
       , parseRepoLife
       , parseLifeConfiguration

         -- * Render 'LifeConfiguration' under @~/.life@
       , renderLifeConfiguration
       , writeGlobalLife
       ) where

import Data.Maybe (maybeToList)
import Fmt (indentF, unlinesF, (+|), (|+))
import Lens.Micro.Platform (makeFields)
import Path (Dir, File, Path, Rel, fromAbsFile, parseRelDir, parseRelFile, toFilePath, (</>))
import Toml (BiToml, Valuer (..), (.=))

import Life.Shell (lifePath, relativeToHome, repoName)

import qualified Data.Set as Set
import qualified Text.Show as Show
import qualified Toml


-- | Data type to represent either file or directory.
data LifePath = File FilePath | Dir FilePath
    deriving (Show)

----------------------------------------------------------------------------
-- Life Configuration data type with lenses
----------------------------------------------------------------------------

data LifeConfiguration = LifeConfiguration
     { lifeConfigurationFiles       :: Set (Path Rel File)
     , lifeConfigurationDirectories :: Set (Path Rel Dir)
     } deriving (Show, Eq)

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
-- LifeConfiguration difference
----------------------------------------------------------------------------

lifeConfigMinus :: LifeConfiguration -- ^ repo .life config
                -> LifeConfiguration -- ^ global config
                -> LifeConfiguration -- ^ configs that are not in global
lifeConfigMinus dotfiles global = LifeConfiguration
    (Set.difference (dotfiles ^. files) (global ^. files))
    (Set.difference (dotfiles ^. directories) (global ^. directories))

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

resurrect :: MonadThrow m => CorpseConfiguration -> m LifeConfiguration
resurrect CorpseConfiguration{..} = do
    filePaths <- mapM parseRelFile corpseFiles
    dirPaths  <- mapM parseRelDir  corpseDirectories

    pure $ LifeConfiguration
        { lifeConfigurationFiles = Set.fromList filePaths
        , lifeConfigurationDirectories = Set.fromList dirPaths
        }

-- TODO: should tomland one day support this?...
-- | Converts 'LifeConfiguration' into TOML file.
renderLifeConfiguration :: Bool  -- ^ True to see empty entries in output
                        -> LifeConfiguration
                        -> Text
renderLifeConfiguration printIfEmpty LifeConfiguration{..} = mconcat $
       maybeToList (render "directories" lifeConfigurationDirectories)
    ++ [ "\n" ]
    ++ maybeToList (render "files" lifeConfigurationFiles)
  where
    render :: Text -> Set (Path b t) -> Maybe Text
    render key paths = do
        let prefix = key <> " = "
        let array  = renderStringArray (length prefix) (map show $ toList paths)

        if not printIfEmpty && null paths
        then Nothing
        else Just $ prefix <> array

    renderStringArray :: Int -> [String] -> Text
    renderStringArray _ []     = "[]"
    renderStringArray n (x:xs) = "[ " +| x |+ "\n"
                              +| indentF n (unlinesF (map (", " ++) xs ++ ["]"]))
                              |+ ""

writeGlobalLife :: LifeConfiguration -> IO ()
writeGlobalLife config = do
    lifeFilePath <- relativeToHome lifePath
    writeFile (fromAbsFile lifeFilePath) (renderLifeConfiguration True config)

----------------------------------------------------------------------------
-- Life configuration parsing
----------------------------------------------------------------------------

parseLifeConfiguration :: MonadThrow m => Text -> m LifeConfiguration
parseLifeConfiguration tomlText = case Toml.decode corpseConfiguationT tomlText of
    Left err  -> throwM $ LoadTomlException (toFilePath lifePath) $ Toml.prettyException err
    Right cfg -> resurrect cfg

-- | Reads 'LifeConfiguration' from @~\/.life@ file.
parseGlobalLife :: IO LifeConfiguration
parseGlobalLife = relativeToHome lifePath >>= readFile . fromAbsFile >>= parseLifeConfiguration

parseRepoLife :: IO LifeConfiguration
parseRepoLife = relativeToHome (repoName </> lifePath)
    >>= readFile . fromAbsFile
    >>= parseLifeConfiguration

data LoadTomlException = LoadTomlException FilePath Text

instance Show.Show LoadTomlException where
    show (LoadTomlException filePath msg) = "Couldnt parse file " ++ filePath ++ ": " ++ show msg

instance Exception LoadTomlException
