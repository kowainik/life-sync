{-# LANGUAGE ApplicativeDo #-}

-- | Command line options for Importify

module Options
       ( LifeCommand (..)
       , InitOptions (..)
       , FileOptions  (..)

       , parseCommand
       ) where

import Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc, help, helper, info,
                            metavar, progDesc, strArgument, subparser)

import Life.Github (Owner (..))

-- | Commands to execute
data LifeCommand
    = Init InitOptions
    | Add  FileOptions
    | Remove FileOptions
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
            (info (helper <*> fmap Add  fileOptionsParser)
                  (fullDesc <> progDesc "Add file or directory to the life configuration."))
 <> command "remove"
            (info (helper <*> fmap Remove  fileOptionsParser)
                  (fullDesc <> progDesc "Remove file or directory from the life configuration."))

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

data FileOptions = FileOptions
     { fileOptionsFile :: FilePath
     } deriving (Show)

fileOptionsParser :: Parser FileOptions
fileOptionsParser = do
    fileOptionsFile <- strArgument
      $ metavar "FILE_PATH"
     <> help "Relative path to file or directory"
    pure FileOptions{..}
