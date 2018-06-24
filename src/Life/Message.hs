{-# LANGUAGE ViewPatterns #-}

module Life.Message
       ( beautyPrint
       , boldText
       , prompt
       , promptNonEmpty
       , errorMessage
       , warningMessage
       , successMessage
       , infoMessage
       , skipMessage
       , abortCmd

         -- * Questions
       , chooseYesNo
       ) where

import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleIntensity (BoldIntensity),
                            ConsoleLayer (Foreground), SGR (..), setSGR)
import System.IO (hFlush)

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Ansi-terminal
----------------------------------------------------------------------------

-- Explicit flush ensures prompt messages are in the correct order on all systems.
putStrFlush :: Text -> IO ()
putStrFlush msg = do
    putText msg
    hFlush stdout

setColor :: Color -> IO ()
setColor color = setSGR [SetColor Foreground Vivid color]

-- | Starts bold printing.
bold :: IO ()
bold = setSGR [SetConsoleIntensity BoldIntensity]

-- | Resets all previous settings.
reset :: IO ()
reset = do
    setSGR [Reset]
    hFlush stdout

-- | Takes list of formatting options, prints text using this format options.
beautyPrint :: [IO ()] -> Text -> IO ()
beautyPrint formats msg = do
    sequence_ formats
    putText msg
    reset

prompt :: IO Text
prompt = do
    setColor Blue
    putStrFlush "  ⟳   "
    reset
    getLine

promptNonEmpty :: IO Text
promptNonEmpty = do
    res <- T.strip <$> prompt
    if null res
        then warningMessage "The answer shouldn't be empty" >> promptNonEmpty
        else pure res


boldText :: Text -> IO ()
boldText message = bold >> putStrFlush message >> reset

boldDefault :: Text -> IO ()
boldDefault message = boldText (" [" <> message <> "]")

colorMessage :: Color -> Text -> IO ()
colorMessage color message = do
    setColor color
    putTextLn $ "  " <> message
    reset

errorMessage, warningMessage, successMessage, infoMessage, skipMessage :: Text -> IO ()
errorMessage   = colorMessage Red
warningMessage = colorMessage Yellow
successMessage = colorMessage Green
infoMessage    = colorMessage Blue
skipMessage    = colorMessage Cyan

-- | Print message and abort current process.
abortCmd :: Text -> Text -> IO ()
abortCmd cmd msg = do
    warningMessage msg
    errorMessage $ "Aborting 'life " <> cmd <> "' command."
    exitFailure

----------------------------------------------------------------------------
-- Questions
----------------------------------------------------------------------------

printQuestion :: Text -> [Text] -> IO ()
printQuestion question (def:rest) = do
    let restSlash = T.intercalate "/" rest
    putStrFlush question
    boldDefault def
    putTextLn $ "/" <> restSlash
printQuestion question [] = putTextLn question

data Answer = Y | N

yesOrNo :: Text -> Maybe Answer
yesOrNo (T.toLower -> answer )
    | T.null answer = Just Y
    | answer `elem` ["yes", "y", "ys"] = Just Y
    | answer `elem` ["no", "n"]  = Just N
    | otherwise = Nothing

chooseYesNo :: Text -> IO Bool
chooseYesNo q = do
    printQuestion q ["y", "n"]
    answer <- yesOrNo <$> prompt
    case answer of
        Nothing -> do
           errorMessage "This wasn't a valid choice."
           chooseYesNo q
        Just Y -> pure True
        Just N -> pure False
