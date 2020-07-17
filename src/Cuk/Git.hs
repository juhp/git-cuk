{-# LANGUAGE ViewPatterns #-}

-- | Logic for CLI commands to make GitHub workflows easier.

module Cuk.Git
       ( runHop
       , runFresh
       , runNew
       , runPush
       , runCommit
       ) where

import Data.Char (isDigit)

import Cuk.ColorTerminal (errorMessage)
import Cuk.Shell (($|))

import qualified Data.Text as T


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

-- | @cuk new@ command.
runNew :: Int -> IO ()
runNew issueNum = do
    login <- "git" $| ["config", "user.login"]
    if login == ""
        then errorMessage "user.login is not specified"
        else do
            let branchName = login <> "/" <> show issueNum
            "git" ["checkout", "-b", branchName]

-- | @cuk commit@ command.
runCommit :: Text -> IO ()
runCommit (T.strip -> msg)
    | msg == "" = errorMessage "Commit message cannot be empty"
    | otherwise = do
        branch <- getCurrentBranch
        let issueNum = issueFromBranch branch
        let prefix = maybe "" showIssueNum issueNum
        "git" ["add", "."]
        "git" ["commit", "-m", prefix <> msg]
  where
    showIssueNum :: Int -> Text
    showIssueNum n = "[#" <> show n <> "] "

-- | @cuk push@ command.
runPush :: IO ()
runPush = getCurrentBranch >>= \branch -> "git" ["push", "-u", "origin", branch]

nameOrMaster :: Maybe Text -> Text
nameOrMaster = fromMaybe "master"

-- | Get the name of the current branch.
getCurrentBranch :: IO Text
getCurrentBranch = "git" $| ["rev-parse", "--abbrev-ref", "HEAD"]

{- | Extracts issue number from the branch in form like:
@
siapbantu/<n>-short-description
@
-}
issueFromBranch :: Text -> Maybe Int
issueFromBranch =
      readMaybe
    . toString
    . T.takeWhile isDigit
    . T.drop 1
    . T.dropWhile (/= '/')
