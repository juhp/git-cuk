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
                            subparser)

import Cuk.ColorTerminal (arrow, blueCode, boldCode, redCode, resetCode)
import Cuk.Git (runFresh, runHop)
import Cuk.Issue (runIssue)

import qualified Data.Text as T
import qualified Paths_git_cuk as Meta (version)


cuk :: IO ()
cuk = execParser cliParser >>= \case
    Hop branchName -> runHop branchName
    Fresh branchName -> runFresh branchName
    Issue issueNum -> runIssue issueNum

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
    | Issue (Maybe Int)

-- | Commands parser.
cukP :: Parser CukCommand
cukP = subparser
    $ command "hop"   (info (helper <*> hopP)   $ progDesc "Switch to branch and sync it")
   <> command "fresh" (info (helper <*> freshP) $ progDesc "Rebase current branch on remote one")
   <> command "issue" (info (helper <*> issueP) $ progDesc "Show the information about the issue")

hopP :: Parser CukCommand
hopP = Hop <$> maybeBranchP

freshP :: Parser CukCommand
freshP = Fresh <$> maybeBranchP

-- | Parse optional branch name as an argument.
maybeBranchP :: Parser (Maybe Text)
maybeBranchP = optional $ strArgument (metavar "BRANCH_NAME")

issueP :: Parser CukCommand
issueP = Issue <$> optional (argument auto (metavar "ISSUE_NUMBER"))

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
