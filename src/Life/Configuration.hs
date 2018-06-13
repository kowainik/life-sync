{-# LANGUAGE ExplicitForAll         #-}
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

         -- * Parsing exceptions
       , ParseLifeException (..)

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
import Path (Dir, File, Path, Rel, fromAbsFile, mkRelFile, parseRelDir, parseRelFile, (</>))
import Path.IO (getHomeDir)
import TOML (Value (..), parseTOML)

import Life.Shell (relativeToHome)

import qualified Data.List as List
import qualified Data.Set as Set

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
-- Life configuration renderer
----------------------------------------------------------------------------

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
--    renderStringArray n [x]    = "[ " +| x |+ " ]"
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

data ParseLifeException = AbsentKeyError Text
                        | WrongTomlError Text
     deriving (Show, Typeable)

instance Exception ParseLifeException

wrongValueError :: Text -> Value -> ParseLifeException
wrongValueError key value = WrongTomlError $
    "Expecting List for key '" <> key <> "' but found: " <> show value

withStringList :: forall m . MonadThrow m => Text -> [(Text, Value)] -> m [String]
withStringList key toml = case List.lookup key toml of
    Nothing          -> throwM $ AbsentKeyError key
    Just (List vals) -> traverse ensureString vals
    Just val         -> throwM $ wrongValueError key val
  where
    ensureString :: Value -> m String
    ensureString (String t) = pure $ toString t
    ensureString value      = throwM $ wrongValueError key value

parseConfigFromToml :: MonadThrow m => [(Text, Value)] -> m LifeConfiguration
parseConfigFromToml toml = do
    lifeFiles <- withStringList "files"       toml
    lifeDirs  <- withStringList "directories" toml

    filePaths <- mapM parseRelFile lifeFiles
    dirPaths  <- mapM parseRelDir  lifeDirs

    return $ LifeConfiguration (Set.fromList filePaths) (Set.fromList dirPaths)

-- | Reads 'LifeConfiguration' from @~\/.life@ file.
parseGlobalLife :: IO LifeConfiguration
parseGlobalLife = do
    homeDirPath <- getHomeDir
    lifeToml    <- readFile (fromAbsFile $ homeDirPath </> lifePath)
    either throwIO parseConfigFromToml $ parseTOML lifeToml
