{-# LANGUAGE ApplicativeDo #-}

-- | Command line options for Importify

module Options
       ( LifeCommand (..)
       , InitOptions (..)
       , PathOptions  (..)

       , parseCommand
       ) where

import Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc, help, helper, info,
                            long, metavar, progDesc, short, strArgument, strOption, subparser)

import Life.Configuration (LifePath (..))
import Life.Github (Owner (..))

-- | Commands to execute
data LifeCommand
    = Init   InitOptions
    | Add    PathOptions
    | Remove PathOptions
    | Push
    deriving (Show)

---------------------------------------------------------------------------
-- Boilerplate
----------------------------------------------------------------------------

commandParser :: Parser LifeCommand
commandParser = subparser $
    command "init"
            (info (helper <*> fmap Init initOptionsParser)
                  (fullDesc <> progDesc "Initialize GitHub repository named 'dotfiles' if you don't have one."))
 <> command "add"
            (info (helper <*> fmap Add pathOptionsParser)
                  (fullDesc <> progDesc "Add file or directory to the life configuration."))
 <> command "remove"
            (info (helper <*> fmap Remove pathOptionsParser)
                  (fullDesc <> progDesc "Remove file or directory from the life configuration."))
 <> command "push"
            (info (helper <*> pure Push)
                  (fullDesc <> progDesc "Updates GitHub repository with the life configuration."))


optionsInfo :: ParserInfo LifeCommand
optionsInfo = info
    (helper  <*> commandParser)
    (fullDesc <> progDesc "life-sync synchronize your personal configs")

parseCommand :: IO LifeCommand
parseCommand = execParser optionsInfo

----------------------------------------------------------------------------
-- life init
----------------------------------------------------------------------------

data InitOptions = InitOptions
     { initOptionsOwner :: Owner
     } deriving (Show)

initOptionsParser :: Parser InitOptions
initOptionsParser = do
    initOptionsOwner <- fmap Owner
      $ strArgument
      $ metavar "OWNER"
     <> help "Your github user name"
    pure InitOptions{..}

----------------------------------------------------------------------------
-- life add
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
