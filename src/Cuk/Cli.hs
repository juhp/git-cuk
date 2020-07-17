{-# LANGUAGE TemplateHaskell #-}

-- | Command line interface for @cuk@ executable.

module Cuk.Cli
       ( cuk
       ) where

import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info,
                            infoOption, long, progDesc, short)

import Cuk.ColorTerminal (blueCode, boldCode, redCode, resetCode)

import qualified Paths_cuk_on as Meta (version)


cuk :: IO ()
cuk = execParser cliParser

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
cliParser :: ParserInfo ()
cliParser = info ( helper <*> versionP <*> cukP )
    $ fullDesc <> progDesc "Haskell Git Helper Tool"

-- | Commands parser.
cukP :: Parser ()
cukP = pass

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
