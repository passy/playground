#!/usr/bin/env stack
-- stack --resolver lts-2.9 --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T
import Prelude hiding (FilePath)
import Turtle
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

excludeList :: [Text]
excludeList =
    [ "node_modules"
    , "bower_components"
    , ".cabal-sandbox"
    , ".ve"
    , "dist"
    , "idea"
    ]

prependToAll :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

printArchives :: IO ()
printArchives = do
    echo "Retrieving archive list ..."
    sh $ do
        archive <- inproc "tarsnap" ["--list-archives"] Turtle.empty
        liftIO $ print archive

createBackup :: Text -> FilePath -> IO ExitCode
createBackup name path = do
    now <- date
    let excludeArgs = prependToAll "--exclude" excludeList
    let fullName = name <> "-" <> T.pack (formatTime defaultTimeLocale "%Y%m%d" now)
    let textPath = either (error "Invalid path") id (toText path)

    let args = [ "-vvv"
               , "--aggressive-networking"
               ] <> excludeArgs <>
               [ "-c"
               , "-f"
               , fullName
               , textPath
               ]
    proc "tarsnap" args Turtle.empty

fsck :: IO ExitCode
fsck = shell "tarsnap --fsck" Turtle.empty

main :: IO ()
main = do
    ecode <- createBackup "projects" "/home/pascal/Projects"
    case ecode of
        ExitSuccess -> return ()
        ExitFailure 1 -> fsck >> main
        ExitFailure e -> die $ format ("Unhandled tarsnap error code: "%d) e
