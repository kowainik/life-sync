{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Contains configuration data type.

module Life.Configuration
       ( LifeConfiguration  (..)
       , ParseLifeException (..)
       , lifePath
       , parseLifeConfiguration
       , renderLifeConfiguration
       ) where

import Universum

import Control.Exception.Base (throwIO)
import Data.List (lookup)
import Fmt (indentF, unlinesF, (+|), (|++|))
import Path (Dir, File, Path, PathException, Rel, fromAbsFile, mkRelFile, parseRelDir, parseRelFile,
             toFilePath, (</>))
import Path.IO (getHomeDir)
import TOML (Value (..), parseTOML)

import qualified Data.Set as Set

data LifeConfiguration = LifeConfiguration
     { lifeConfigurationFiles       :: Set (Path Rel File)
     , lifeConfigurationDirectories :: Set (Path Rel Dir)
     } deriving (Show)

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
        let array  = renderStringArray (length prefix) (map toFilePath $ toList paths)
        prefix <> array

    renderStringArray :: Int -> [String] -> Text
    renderStringArray _ []     = "[]"
    renderStringArray n (x:xs) = "[ " +| x
                            |++| indentF n (unlinesF (map (", " ++) xs)
                              +| "]")

data ParseLifeException = AbsentKeyError Text
                        | WrongTomlError Text
                        | PathParseError PathException
     deriving (Show, Typeable)

instance Exception ParseLifeException

wrongValueError :: Text -> Value -> ParseLifeException
wrongValueError key value = WrongTomlError $
    "Expecting List for key '" <> key <> "' but found: " <> show value

withStringList :: forall m . MonadThrow m => Text -> [(Text, Value)] -> m [String]
withStringList key toml = case lookup key toml of
    Nothing          -> throwM $ AbsentKeyError key
    Just (List vals) -> traverse ensureString vals
    Just val         -> throwM $ wrongValueError key val
  where
    ensureString :: Value -> m String
    ensureString (String t) = pure $ toString t
    ensureString value      = throwM $ wrongValueError key value

parseConfigFromToml :: MonadThrow m => [(Text, Value)] -> m LifeConfiguration
parseConfigFromToml toml = do
    files       <- withStringList "files"       toml
    directories <- withStringList "directories" toml

    filePaths <- mapM parseRelFile files
    dirPaths  <- mapM parseRelDir  directories

    return $ LifeConfiguration (Set.fromList filePaths) (Set.fromList dirPaths)

-- | Name for life configuration file.
lifePath :: Path Rel File
lifePath = $(mkRelFile ".life")

-- | Reads 'LifeConfiguration' from @~\/.life@ file.
parseLifeConfiguration :: IO LifeConfiguration
parseLifeConfiguration = do
    homeDirPath <- getHomeDir
    lifeToml    <- readFile (fromAbsFile $ homeDirPath </> lifePath)
    either throwIO parseConfigFromToml $ parseTOML lifeToml
