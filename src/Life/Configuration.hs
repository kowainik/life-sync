{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Contains configuration data type.

module Life.Configuration
       ( LifeConfiguration  (..)

       , getBranch
       , getBranchName
       , singleDirConfig
       , singleFileConfig
       , lifeConfigMinus

--         -- * Parsing exceptions
--       , ParseLifeException (..)

         -- * Lenses for 'LifeConfiguration'
       , branch
       , files
       , directories

         -- * Parse 'LifeConfiguration' under @~/.life@
       , parseHomeLife
       , parseRepoLife
       , parseLifeConfiguration

         -- * Render 'LifeConfiguration' under @~/.life@
       , renderLifeConfiguration
       , writeGlobalLife
       ) where

import Control.Monad.Catch (MonadThrow (..))
import Fmt (indentF, unlinesF, (+|), (|+))
import Lens.Micro.Platform (makeFields, (.~), (^.))
import Path (Dir, File, Path, Rel, fromAbsFile, parseRelDir, parseRelFile, toFilePath, (</>))
import Toml (AnyValue (..), BiToml, Prism (..), (.=))

import Life.Shell (lifePath, relativeToHome, repoName)
import Life.Core (Branch (..))

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Relude.Unsafe as Unsafe
import qualified Text.Show as Show
import qualified Toml


----------------------------------------------------------------------------
-- Life Configuration data type with lenses
----------------------------------------------------------------------------

data LifeConfiguration = LifeConfiguration
     { lifeConfigurationFiles       :: Set (Path Rel File)
     , lifeConfigurationDirectories :: Set (Path Rel Dir)
     , lifeConfigurationBranch      :: Last Branch
     } deriving (Show, Eq)

makeFields ''LifeConfiguration

getBranch :: LifeConfiguration -> Branch
getBranch  = Unsafe.fromJust . getLast . lifeConfigurationBranch

getBranchName :: LifeConfiguration -> Text
getBranchName = unBranch . getBranch

----------------------------------------------------------------------------
-- Algebraic instances and utilities
----------------------------------------------------------------------------

instance Semigroup LifeConfiguration where
    life1 <> life2 = LifeConfiguration
        { lifeConfigurationFiles       = life1^.files <> life2^.files
        , lifeConfigurationDirectories = life1^.directories <> life2^.directories
        , lifeConfigurationBranch      = life2^.branch
        }

instance Monoid LifeConfiguration where
    mempty  = LifeConfiguration mempty mempty mempty
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
    (lifeConfigurationBranch dotfiles)

----------------------------------------------------------------------------
-- Toml parser for life configuration
----------------------------------------------------------------------------

data CorpseConfiguration = CorpseConfiguration
    { corpseFiles       :: [FilePath]
    , corpseDirectories :: [FilePath]
    , corpseBranch      :: Text
    }

corpseConfiguationT :: BiToml CorpseConfiguration
corpseConfiguationT = CorpseConfiguration
    <$> Toml.arrayOf _String "files"       .= corpseFiles
    <*> Toml.arrayOf _String "directories" .= corpseDirectories
    <*> Toml.text            "branch"      .= corpseBranch
  where
    _String :: Prism AnyValue String
    _String = Prism
        { preview = \(AnyValue t) -> toString <$> Toml.matchText t
        , review = AnyValue . Toml.Text . toText
        }

resurrect :: MonadThrow m => CorpseConfiguration -> m LifeConfiguration
resurrect CorpseConfiguration{..} = do
    filePaths <- mapM parseRelFile corpseFiles
    dirPaths  <- mapM parseRelDir  corpseDirectories

    pure $ LifeConfiguration
        { lifeConfigurationFiles = Set.fromList filePaths
        , lifeConfigurationDirectories = Set.fromList dirPaths
        , lifeConfigurationBranch = Last $ Just $ Branch corpseBranch
        }

-- TODO: should tomland one day support this?...
-- | Converts 'LifeConfiguration' into TOML file.
renderLifeConfiguration :: Bool  -- ^ True to see empty entries in output
                        -> LifeConfiguration
                        -> Text
renderLifeConfiguration printIfEmpty life@LifeConfiguration{..} = T.intercalate "\n"
       [ "branch = \"" <> getBranchName life <> "\""
       , mconcat $ maybeToList (render "directories" lifeConfigurationDirectories)
       , mconcat $ maybeToList (render "files" lifeConfigurationFiles)]
  where
    render :: Text -> Set (Path b t) -> Maybe Text
    render key paths = do
        let prefix = key <> " = "
        let array  = renderStringArray (T.length prefix) (map show $ toList paths)

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

parseLife :: Path Rel File -> IO LifeConfiguration
parseLife path = relativeToHome path
             >>= readFile . fromAbsFile
             >>= parseLifeConfiguration

-- | Reads 'LifeConfiguration' from @~\/.life@ file.
parseHomeLife :: IO LifeConfiguration
parseHomeLife = parseLife lifePath

-- | Reads 'LifeConfiguration' from @~\/dotfiles\/.life@ file.
parseRepoLife :: IO LifeConfiguration
parseRepoLife = parseLife (repoName </> lifePath)

data LoadTomlException = LoadTomlException FilePath Text

instance Show.Show LoadTomlException where
    show (LoadTomlException filePath msg) = "Couldnt parse file " ++ filePath ++ ": " ++ show msg

instance Exception LoadTomlException
