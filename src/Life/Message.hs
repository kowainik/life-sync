{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains functions for colorful printing, prompt and other terminal related
messages.
-}

module Life.Message
    ( prompt
    , promptNonEmpty
    , abortCmd

      -- * Questions
    , choose
    , chooseYesNo
    ) where

import Colourista (blue, bold, errorMessage, formatWith, warningMessage)
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

choose :: Text -> NonEmpty Text -> IO Text
choose question choices = do
    printQuestion question $ toList choices
    answer <- prompt
    if | T.null answer -> pure (head choices)
       | T.toLower answer `elem` choices -> pure answer
       | otherwise -> do
           errorMessage "This wasn't a valid choice."
           choose question choices

data Answer
    = Y
    | N

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
