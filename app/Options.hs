{-# LANGUAGE ApplicativeDo #-}

-- | Command line options for Importify

module Options
       ( LifeCommand (..)
       , InitOptions (..)
       , AddOptions  (..)

       , parseCommand
       ) where

import Universum

import Options.Applicative (Parser, ParserInfo, auto, command, execParser, flag', fullDesc, help,
                            helper, info, long, metavar, option, progDesc, short, showDefault,
                            strArgument, strOption, subparser, switch, value)

import Life.Github (Owner (..))

-- | Commands to execute
data LifeCommand
    = Init InitOptions
    | Add  AddOptions
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
            (info (helper <*> fmap Add  addOptionsParser)
                  (fullDesc <> progDesc "Add file or directory to life configuration."))

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

data AddOptions = AddOptions
     { addOptionsFile :: FilePath
     } deriving (Show)

addOptionsParser :: Parser AddOptions
addOptionsParser = do
    addOptionsFile <- strArgument
      $ metavar "FILE_PATH"
     <> help "Path to file or directory"
    pure AddOptions{..}
