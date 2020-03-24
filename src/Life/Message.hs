{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE ViewPatterns #-}

module Life.Message
       ( prompt
       , promptNonEmpty
       , errorMessage
       , warningMessage
       , successMessage
       , infoMessage
       , skipMessage
       , abortCmd

         -- * Questions
       , choose
       , chooseYesNo
       ) where

import Colourista (blue, bold, formatWith)
import System.IO (hFlush)

import qualified Colourista
import qualified Data.Text as T
import qualified Relude.Unsafe as Unsafe

----------------------------------------------------------------------------
-- Ansi-terminal
----------------------------------------------------------------------------

-- Explicit flush ensures prompt messages are in the correct order on all systems.
putStrFlush :: Text -> IO ()
putStrFlush msg = do
    putText msg
    hFlush stdout

{- | Read 'Text' from standard input after arrow prompt.
-}
prompt :: IO Text
prompt = do
    putStrFlush $ formatWith [blue] "  ⟳   "
    getLine

promptNonEmpty :: IO Text
promptNonEmpty = do
    res <- T.strip <$> prompt
    if T.null res
        then warningMessage "The answer shouldn't be empty" >> promptNonEmpty
        else pure res

boldDefault :: Text -> Text
boldDefault message = formatWith [bold] $ " [" <> message <> "]"

errorMessage, warningMessage, successMessage, infoMessage, skipMessage :: Text -> IO ()
errorMessage   = Colourista.errorMessage   . indent
warningMessage = Colourista.warningMessage . indent
successMessage = Colourista.successMessage . indent
infoMessage    = Colourista.infoMessage    . indent
skipMessage    = Colourista.skipMessage    . indent

-- | Add 2 spaces in front.
indent :: Text -> Text
indent = ("  " <>)

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
printQuestion question [] = putTextLn question
printQuestion question (def:rest) = do
    let restSlash = T.intercalate "/" rest
    putStrFlush $ question <> boldDefault def
    putTextLn $ "/" <> restSlash

choose :: Text -> [Text] -> IO Text
choose question choices = do
    printQuestion question choices
    answer <- prompt
    if | T.null answer -> pure (Unsafe.head choices)
       | T.toLower answer `elem` choices -> pure answer
       | otherwise -> do
           errorMessage "This wasn't a valid choice."
           choose question choices

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
