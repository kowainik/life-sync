-- | Contains configuration data type.

module Life.Configuration
       ( LifeConfiguration (..)
       , singleDirConfig
       , singleFileConfig
       , defaultLifeConfig

       , lifeConfigMinus

--         -- * Parsing exceptions
--       , ParseLifeException (..)

         -- * Lenses for 'LifeConfiguration'
       , filesL
       , directoriesL
       , branchL

         -- * Parse 'LifeConfiguration' under @~/.life@
       , parseHomeLife
       , parseRepoLife
       , parseLifeConfiguration

         -- * Render 'LifeConfiguration' under @~/.life@
       , renderLifeConfiguration
       , writeGlobalLife
       ) where

import Control.Monad.Catch (MonadThrow (..))
import Path (Dir, File, Path, Rel, fromAbsFile, parseRelDir, parseRelFile, toFilePath, (</>))
import Relude.Extra.Lens (Lens', lens, (.~), (^.))
import Toml (TomlCodec, (.=))

import Life.Core (Branch (..), master)
import Life.Path (lifePath, relativeToHome, repoName)

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Show as Show
import qualified Toml


-- | The configurations data type.
data LifeConfiguration = LifeConfiguration
     { lifeConfigurationFiles       :: !(Set (Path Rel File))
     , lifeConfigurationDirectories :: !(Set (Path Rel Dir))
     , lifeConfigurationBranch      :: !(Last Branch)
     } deriving stock (Show, Eq)

-- | Lens for 'lifeConfigurationFiles'.
filesL :: Lens' LifeConfiguration (Set (Path Rel File))
filesL = lens
    lifeConfigurationFiles
    (\config newFiles -> config {lifeConfigurationFiles = newFiles})

-- | Lens for 'lifeConfigurationDirectories'.
directoriesL :: Lens' LifeConfiguration (Set (Path Rel Dir))
directoriesL = lens
    lifeConfigurationDirectories
    (\config newDirs -> config {lifeConfigurationDirectories = newDirs})

-- | Lens for 'lifeConfigurationBranch'.
branchL :: Lens' LifeConfiguration (Last Branch)
branchL = lens
    lifeConfigurationBranch
    (\config newBr -> config {lifeConfigurationBranch = newBr})

----------------------------------------------------------------------------
-- Algebraic instances and utilities
----------------------------------------------------------------------------

instance Semigroup LifeConfiguration where
    life1 <> life2 = LifeConfiguration
        { lifeConfigurationFiles       = (life1 ^. filesL) <> (life2 ^. filesL)
        , lifeConfigurationDirectories = (life1 ^. directoriesL) <> (life2 ^. directoriesL)
        , lifeConfigurationBranch      = (life1 ^. branchL) <> (life2 ^. branchL)
        }

instance Monoid LifeConfiguration where
    mempty  = LifeConfiguration mempty mempty mempty
    mappend = (<>)

-- | The defaulting 'LifeConfiguration', with the default @master@ branch.
defaultLifeConfig :: LifeConfiguration
defaultLifeConfig = LifeConfiguration
    { lifeConfigurationFiles       = mempty
    , lifeConfigurationDirectories = mempty
    , lifeConfigurationBranch      = Last $ Just master
    }

-- | Creates a 'LifeConfiguration' with the given file.
singleFileConfig :: Path Rel File -> LifeConfiguration
singleFileConfig file = mempty & filesL .~ one file

-- | Creates a 'LifeConfiguration' with the given folder.
singleDirConfig :: Path Rel Dir -> LifeConfiguration
singleDirConfig dir = mempty & directoriesL .~ one dir

----------------------------------------------------------------------------
-- LifeConfiguration difference
----------------------------------------------------------------------------

lifeConfigMinus
    :: LifeConfiguration -- ^ Repository @.life@ configuration
    -> LifeConfiguration -- ^ Global configuration
    -> LifeConfiguration -- ^ Configuration that is not in global
lifeConfigMinus dotfiles global = LifeConfiguration
    (Set.difference (dotfiles ^. filesL) (global ^. filesL))
    (Set.difference (dotfiles ^. directoriesL) (global ^. directoriesL))
    (Last $ Just master)

----------------------------------------------------------------------------
-- Toml parser for life configuration
----------------------------------------------------------------------------

data CorpseConfiguration = CorpseConfiguration
    { corpseFiles       :: [FilePath]
    , corpseDirectories :: [FilePath]
    }

corpseConfiguationT :: TomlCodec CorpseConfiguration
corpseConfiguationT = CorpseConfiguration
    <$> Toml.arrayOf Toml._String "files"       .= corpseFiles
    <*> Toml.arrayOf Toml._String "directories" .= corpseDirectories

resurrect :: MonadThrow m => CorpseConfiguration -> m LifeConfiguration
resurrect CorpseConfiguration{..} = do
    filePaths <- mapM parseRelFile corpseFiles
    dirPaths  <- mapM parseRelDir  corpseDirectories

    pure $ LifeConfiguration
        { lifeConfigurationFiles = Set.fromList filePaths
        , lifeConfigurationDirectories = Set.fromList dirPaths
        , lifeConfigurationBranch = Last (Just master)
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
        let array  = renderStringArray $ map show $ toList paths
        if not printIfEmpty && null paths
        then Nothing
        else Just $ prefix <> array

    renderStringArray :: [Text] -> Text
    renderStringArray = \case
        [] -> "[]"
        [x] -> "[" <> x <> "]"
        l -> mconcat
            [ "\n    [ "
            , T.intercalate "\n    , " l
            , "\n    ]"
            ]

writeGlobalLife :: LifeConfiguration -> IO ()
writeGlobalLife config = do
    lifeFilePath <- relativeToHome lifePath
    writeFileText (fromAbsFile lifeFilePath) (renderLifeConfiguration True config)

----------------------------------------------------------------------------
-- Life configuration parsing
----------------------------------------------------------------------------

parseLifeConfiguration :: MonadThrow m => Text -> m LifeConfiguration
parseLifeConfiguration tomlText = case Toml.decode corpseConfiguationT tomlText of
    Left err  -> throwM $ LoadTomlException (toFilePath lifePath) $ Toml.prettyException err
    Right cfg -> resurrect cfg

parseLife :: Path Rel File -> IO LifeConfiguration
parseLife path = relativeToHome path
             >>= readFileText . fromAbsFile
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
