{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line interface for @cuk@ executable.

module Cuk.Cli
       ( cuk
       ) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc, help, helper, info,
                            infoOption, long, metavar, progDesc, short, strArgument, subparser)

import Cuk.ColorTerminal (blueCode, boldCode, redCode, resetCode)
import Cuk.Git (runHop)

import qualified Paths_git_cuk as Meta (version)


cuk :: IO ()
cuk = execParser cliParser >>= \case
    Hop branchName -> runHop branchName

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
cliParser :: ParserInfo CukCommand
cliParser = info ( helper <*> versionP <*> cukP )
    $ fullDesc <> progDesc "Haskell Git Helper Tool"

-- | Commands for
newtype CukCommand
    = Hop (Maybe Text)

-- | Commands parser.
cukP :: Parser CukCommand
cukP = subparser
    $ command "hop" (info (helper <*> hopP) $ progDesc "Switch to branch and sync it")

hopP :: Parser CukCommand
hopP = do
    branchName <- optional $ strArgument (metavar "BRANCH_NAME")
    pure $ Hop branchName

-- | Show the version of the tool.
versionP :: Parser (a -> a)
versionP = infoOption cukVersion
    $ long "version"
   <> short 'v'
   <> help "Show cuk's version"

cukVersion :: String
cukVersion = toString
    $ intercalate "\n"
    $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    sVersion = blueCode <> boldCode <> "Cuk " <> "v" <>  showVersion Meta.version <> resetCode
    sHash = " ➤ " <> blueCode <> boldCode <> "Git revision: " <> resetCode <> $(gitHash)
    sDate = " ➤ " <> blueCode <> boldCode <> "Commit date:  " <> resetCode <> $(gitCommitDate)
    sDirty = redCode <> "There are non-committed files." <> resetCode
