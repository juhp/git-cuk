{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line interface for @cuk@ executable.

module Cuk.Cli
       ( cuk
       ) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import Options.Applicative (Parser, ParserInfo, argument, auto, command, execParser, fullDesc, help,
                            helper, info, infoOption, long, metavar, progDesc, short, strArgument,
                            subparser, switch)

import Cuk.ColorTerminal (arrow, blueCode, boldCode, redCode, resetCode)
import Cuk.Git (runCommit, runCurrent, runFix, runAmend, runFresh, runHop, runNew, runPush, runResolve,
                runSync)
import Cuk.Issue (runIssue)

import qualified Data.Text as T
import qualified Paths_git_cuk as Meta (version)


cuk :: IO ()
cuk = execParser cliParser >>= \case
    Hop branchName -> runHop branchName
    Fresh branchName -> runFresh branchName
    New issueNum -> runNew issueNum
    Issue issueNum -> runIssue issueNum
    Commit message noIssue -> runCommit message noIssue
    Fix message -> runFix message
    Amend -> runAmend
    Resolve branchName -> runResolve branchName
    Push isForce -> runPush isForce
    Sync -> runSync
    Current -> runCurrent >>= flip whenJust (runIssue . Just)

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
cliParser :: ParserInfo CukCommand
cliParser = info ( helper <*> versionP <*> cukP )
    $ fullDesc <> progDesc "Haskell Git Helper Tool"

-- | Commands for
data CukCommand
    = Hop (Maybe Text)
    | Fresh (Maybe Text)
    | New Int
    | Issue (Maybe Int)
    | Commit Text Bool
    | Fix (Maybe Text)
    | Amend
    | Resolve (Maybe Text)
    | Push Bool
    | Sync
    | Current

-- | Commands parser.
cukP :: Parser CukCommand
cukP = subparser
    $ command "hop"     (info (helper <*> hopP)     $ progDesc "Switch to branch and sync it")
   <> command "fresh"   (info (helper <*> freshP)   $ progDesc "Rebase current branch on remote one")
   <> command "new"     (info (helper <*> newP)     $ progDesc "Create new branch from current one")
   <> command "commit"  (info (helper <*> commitP)  $ progDesc "Commit all local changes and prepend issue number")
   <> command "fix"     (info (helper <*> fixP)     $ progDesc "Fix requested changes to the last commit")
   <> command "amend"   (info (helper <*> amendP)   $ progDesc "Amend changes to the last commit and force push")
   <> command "issue"   (info (helper <*> issueP)   $ progDesc "Show the information about the issue")
   <> command "push"    (info (helper <*> pushP)    $ progDesc "Push the current branch")
   <> command "sync"    (info (helper <*> syncP)    $ progDesc "Sync local branch with its remote")
   <> command "resolve" (info (helper <*> resolveP) $ progDesc "Switch to master, sync and delete the branch")
   <> command "current" (info (helper <*> currentP) $ progDesc "Show info about current branch and issue (if applicable)")

hopP :: Parser CukCommand
hopP = Hop <$> maybeBranchP

freshP :: Parser CukCommand
freshP = Fresh <$> maybeBranchP

newP :: Parser CukCommand
newP = New <$> issueNumP

issueP :: Parser CukCommand
issueP = Issue <$> optional issueNumP

commitP :: Parser CukCommand
commitP = do
    msg <- strArgument (metavar "COMMIT_MESSAGE")
    noIssue <- switch
        $ long "no-issue"
       <> short 'n'
       <> help "Do not add [#ISSUE_NUMBER] prefix when specified"
    pure $ Commit msg noIssue

fixP :: Parser CukCommand
fixP = Fix <$> commitMessageP

amendP :: Parser CukCommand
amendP = pure Amend

pushP :: Parser CukCommand
pushP = Push <$> switch
    ( long "force"
   <> short 'f'
   <> help "Force push"
    )

syncP :: Parser CukCommand
syncP = pure Sync

currentP :: Parser CukCommand
currentP = pure Current

resolveP :: Parser CukCommand
resolveP = Resolve <$> maybeBranchP

-- | Parse optional branch name as an argument.
maybeBranchP :: Parser (Maybe Text)
maybeBranchP = optional $ strArgument (metavar "BRANCH_NAME")

-- / Parse optional commit message as an argument
commitMessageP :: Parser (Maybe Text)
commitMessageP = optional $ strArgument (metavar "COMMIT_MESSAGE")

-- | Parse issue number as an argument.
issueNumP :: Parser Int
issueNumP = argument auto $ metavar "ISSUE_NUMBER"

-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption cukVersion
    $ long "version"
   <> short 'v'
   <> help "Show cuk's version"

cukVersion :: String
cukVersion = toString
    $ T.intercalate "\n"
    $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    blueBold txt = blueCode <> boldCode <> txt <> resetCode
    sVersion = blueBold "Cuk " <> "v" <> toText (showVersion Meta.version)
    sHash = arrow <> blueBold "Git revision: " <> $(gitHash)
    sDate = arrow <> blueBold "Commit date:  " <> $(gitCommitDate)
    sDirty = redCode <> "There are non-committed files." <> resetCode
