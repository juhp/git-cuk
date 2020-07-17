-- | Logic for CLI commands to make GitHub workflows easier.

module Cuk.Git
       ( runHop
       ) where

import Cuk.Shell ()


-- | @cuk hop@ command
runHop :: Maybe Text -> IO ()
runHop branchName = do
    let branch = fromMaybe "master" branchName
    "git" ["checkout",  branch]
    "git" ["pull", "--rebase", "--prune"]
