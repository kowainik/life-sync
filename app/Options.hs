{-# LANGUAGE ApplicativeDo #-}

-- | Command line options for Importify

module Options
       ( LifeCommand (..)
       , PathOptions (..)
       , PullOptions (..)

       , parseCommand
       ) where

import Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc, help, helper, info,
                            long, metavar, progDesc, short, strArgument, strOption, subparser)

import Life.Configuration (LifePath (..))
import Life.Github (Owner (..))

-- | Commands to execute
data LifeCommand
    = Init   Owner
    | Add    PathOptions
    | Remove PathOptions
    | Push
    | Pull   PullOptions
    deriving (Show)

---------------------------------------------------------------------------
-- Boilerplate
----------------------------------------------------------------------------

commandParser :: Parser LifeCommand
commandParser = subparser $
    command "init"
            (info (helper <*> fmap Init ownerParser)
                  (fullDesc <> progDesc "Initialize GitHub repository named 'dotfiles' if you don't have one."))
 <> command "add"
            (info (helper <*> fmap Add pathOptionsParser)
                  (fullDesc <> progDesc "Add file or directory to the life configuration."))
 <> command "remove"
            (info (helper <*> fmap Remove pathOptionsParser)
                  (fullDesc <> progDesc "Remove file or directory from the life configuration."))
 <> command "push"
            (info (helper <*> pure Push)
                  (fullDesc <> progDesc "Updates GitHub repository from local state and push the latest version."))
 <> command "pull"
            (info (helper <*> fmap Pull pullOptionsParser)
                  (fullDesc <> progDesc "Updates local state of '.life' and 'dotfiles' from GitHub repository."))


optionsInfo :: ParserInfo LifeCommand
optionsInfo = info
    (helper  <*> commandParser)
    (fullDesc <> progDesc "life-sync synchronize your personal configs")

parseCommand :: IO LifeCommand
parseCommand = execParser optionsInfo

ownerParser :: Parser Owner
ownerParser = fmap Owner
     $ strArgument
     $ metavar "OWNER"
    <> help "Your github user name"

----------------------------------------------------------------------------
-- life pull
----------------------------------------------------------------------------

data PullOptions = PullOptions
    { pullOptionsOwner   :: Owner
    , pullOptionsNoFiles :: [FilePath]
    , pullOptionsNoDirs  :: [FilePath]
    } deriving (Show)

pullOptionsParser :: Parser PullOptions
pullOptionsParser = do
    pullOptionsOwner <- ownerParser

    -- TODO: reuse LifePath parser here?...
    pullOptionsNoFiles <- many $ strOption
                        $ metavar "FILE_PATH"
                       <> long "no-file"
                       <> short 'f'
                       <> help "Excluding these specific files from copying"

    pullOptionsNoDirs <- many $ strOption
                       $ metavar "FILE_PATH"
                      <> long "no-dir"
                      <> short 'd'
                      <> help "Excluding these specific directories from copying"

    pure PullOptions{..}

----------------------------------------------------------------------------
-- life add and remove
----------------------------------------------------------------------------

data PathOptions = PathOptions
     { pathOptionsPath :: LifePath
     } deriving (Show)

pathOptionsParser :: Parser PathOptions
pathOptionsParser = do
    pathOptionsPath <- fileParser <|> dirParser
    pure PathOptions{..}
  where
    fileParser :: Parser LifePath
    fileParser = File <$> strOption
                        ( metavar "FILE_PATH"
                       <> long "file"
                       <> short 'f'
                        )

    dirParser :: Parser LifePath
    dirParser = Dir <$> strOption
                        ( metavar "DIRECTORY_PATH"
                       <> long "dir"
                       <> short 'd'
                        )
