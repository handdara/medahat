{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Mdh
import Turtle
import Prelude hiding (FilePath)

-- # Argument Parsing

optsParser :: Parser Opts
optsParser = Opts <$> switch "verbose" 'v' "Verbose mode allows logs"

showNotesParser :: Parser MdhCommands
showNotesParser = ShowNotes <$> some (argPath "nodes" "nodes in collection tree" )

cmdsParser :: Parser MdhCommands
cmdsParser =
  subcommand "tree" "Show note structure" (pure ShowTree)
    <|> subcommand "qw" "Quick open daily work notes" (pure QuickWork)
    <|> subcommand "qp" "Quick open daily work notes" (pure QuickPersonal)
    <|> subcommand "show" "Show notes at a directory" showNotesParser

argParser :: Parser Command
argParser =
  Command
    <$> cmdsParser
    <*> optsParser

-- # Main Func

mdh :: (MonadIO io) => Config -> io ()
mdh c = do
  (Command mCfg mOpts) <- options "Medahat, markdown notes utility for handdara" argParser
  mdhLog mOpts "Config loaded successfully, running"
  mdhLog mOpts "Not currently using 'openLine' config option in this version of mdh"

  case mCfg of
    ShowTree -> do
      mt <- getTree c
      case mt of
        Just t  -> stdout $ strTreeToShell t
        Nothing -> mdhError "File tree loaded incorrectly"
    QuickWork -> do
      absDir <- absoluteMdhDir c <&> (</> "work")
      mktree absDir
      let absFile = absDir </> "daily" <.> "md"
      touch absFile
      procs (fromString $ editor c) [fromString absFile] empty
    QuickPersonal -> do
      absDir <- absoluteMdhDir c <&> (</> "personal")
      mktree absDir
      let absFile = absDir </> "daily" <.> "md"
      touch absFile
      procs (fromString $ editor c) [fromString absFile] empty
    ShowNotes ns -> do
      mt <- getTree c
      let mpt = mt >>= nodeSearch ns
      case mpt of
        Just (p, _) -> stdout $ mdsAtRelDir c p
        Nothing -> mdhDie "Couldn't find collection"

    _ -> mdhDie "not implemented"

  return ()

main :: IO ()
main =  do
  -- Attempt to get config
  cfgAttempt <- liftIO getConfig
  case cfgAttempt of
    Nothing -> liftIO $ putStrLn "Couldn't load config file, check ~/.config/mdh/"
    Just config -> mdh config
