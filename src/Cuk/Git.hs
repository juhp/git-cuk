{-# LANGUAGE ViewPatterns #-}

-- | Logic for CLI commands to make GitHub workflows easier.

module Cuk.Git
       ( runHop
       , runFresh
       ) where

import Cuk.Shell ()


-- | @cuk hop@ command.
runHop :: Maybe Text -> IO ()
runHop (nameOrMaster -> branch) = do
    "git" ["checkout",  branch]
    "git" ["pull", "--rebase", "--prune"]

-- | @cuk fresh@ command.
runFresh :: Maybe Text -> IO ()
runFresh (nameOrMaster -> branch) = do
    "git" ["fetch", "origin", branch]
    "git" ["rebase", "origin/" <> branch]

nameOrMaster :: Maybe Text -> Text
nameOrMaster = fromMaybe "master"
