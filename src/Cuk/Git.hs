{-# LANGUAGE ViewPatterns #-}

-- | Logic for CLI commands to make GitHub workflows easier.

module Cuk.Git
       ( runHop
       , runFresh
       , runNew
       , runPush
       , runResolve
       , runCommit
       , runFix
       , runAmend
       , runSync
       , runCurrent
       , runClone
       ) where

import Data.Char (isAlphaNum, isDigit, isSpace)

import Cuk.ColorTerminal (arrow, errorMessage, greenCode, resetCode)
import Cuk.Issue (getIssueTitle, mkIssueId)
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
    login <- getUsername
    let issueId = mkIssueId issueNum
    issueTitle <- getIssueTitle issueId
    let shortDesc = mkShortDesc issueTitle
    let branchName = login <> "/" <> show issueNum <> "-" <> shortDesc
    "git" ["checkout", "-b", branchName]
  where
    mkShortDesc :: Text -> Text
    mkShortDesc =
          T.intercalate "-"
        . take 5
        . words
        . T.filter (\c -> isAlphaNum c || isDigit c || isSpace c)

-- | @cuk commit@ command.
runCommit :: Text -> Bool -> IO ()
runCommit (T.strip -> msg) (not -> hasIssue)
    | msg == "" = errorMessage "Commit message cannot be empty"
    | otherwise = do
        branch <- getCurrentBranch
        let issueNum = issueFromBranch branch
        "git" ["add", "."]
        "git" ["commit", "-m", showMsg $ guard hasIssue *> issueNum]
  where
    showMsg :: Maybe Int -> Text
    showMsg = \case
       Nothing -> msg
       Just n  ->
           let issue = "#" <> show n
           in "[" <> issue <> "] " <> msg <> "\n\nResolves " <> issue

-- / @cuk fix@ command
runFix :: Maybe Text -> IO ()
runFix msg = do
    "git" ["add", "."]
    "git" ["commit", "-m", message]
    runPush False
    where
        message = fromMaybe "Fix after review" msg

-- | @cuk amend@ command.
runAmend :: IO ()
runAmend = do
    "git" ["add", "."]
    "git" ["commit", "--amend", "--no-edit"]
    runPush True

-- | @cuk push@ command.
runPush :: Bool -> IO ()
runPush isForce = getCurrentBranch >>= \branch ->
    "git" $ ["push", "--set-upstream", "origin", branch]
         ++ ["--force" | isForce]

-- | @cuk sync@ command.
runSync :: IO ()
runSync = getCurrentBranch >>= \branch -> "git" ["pull", "--rebase", "origin", branch]

-- | @cuk resolve@ command.
runResolve :: Maybe Text -> IO ()
runResolve (nameOrMaster -> master)= do
    curBranch <- getCurrentBranch
    runHop $ Just master
    when (curBranch /= master) $ "git" ["branch", "-D", curBranch]

{- | Part of the @cuk current@ command. Prints the current branch and returns
the current issue number if possible.
-}
runCurrent :: IO (Maybe Int)
runCurrent = do
    branchName <- getCurrentBranch
    putTextLn $ arrow <> "Current branch: " <> greenCode <> branchName <> resetCode
    pure $ issueFromBranch branchName

{- | @cuk clone@ command receives the name of the repo in the following
formats:
* @reponame@ — current user's username is used to clone the repo from.
* @name/reponame@ — specified GitHub username is used to clone the repo from.
__Note__ that the @ssh@ strategy is used for cloning from GitHub. See the corresponding @git@ command:
@
git clone git@github.com:username/project-name.git
@
-}
runClone :: Text -> IO ()
runClone txt = do
    name <- case T.splitOn "/" txt of
        [reponame] -> getUsername >>= \u -> pure $ u <> "/" <> reponame
        [username, reponame] -> pure $ username <> "/" <> reponame
        _ -> do
            errorMessage ("Incorrect name: " <> txt <> ". Use 'repo' or 'user/repo' formats")
            exitFailure
    let gitLink = "git@github.com:" <> name <> ".git"
    "git" ["clone", gitLink]

----------------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------------

-- | Get current user name from the local global git config.
getUsername :: IO Text
getUsername = do
    login <- "git" $| ["config", "user.login"]
    if login == ""
        then errorMessage "user.login is not specified" >> exitFailure
        else pure login

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
