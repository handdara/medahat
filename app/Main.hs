{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Mdh
import Turtle
import Prelude hiding (FilePath)

-- # Argument Parsing

optsParser :: Parser Opts
optsParser = Opts <$>            switch  "verbose" 'v' "Verbose mode allows logs"
                  <*> optional ( optPath "config"  'c' "Provide config file" )

showNotesParser :: Parser MdhCommands
showNotesParser = ShowNotes <$> some (argPath "nodes" "nodes in collection tree")

openNoteParser :: Parser MdhCommands
openNoteParser = undefined

cmdsParser :: Parser MdhCommands
cmdsParser =
  subcommand "tree" "Show note structure" (pure ShowTree)
    <|> subcommandGroup
      "Quickly open daily notes:"
      [ ("qw", "Quick open daily work notes", pure QuickWork),
        ("qp", "Quick open daily work notes", pure QuickPersonal)
      ]
    <|> subcommandGroup
      "Show commands:"
      [ ("show", "Find and show notes at a collection or sub-collection", showNotesParser),
        ("s", "\talias for show", showNotesParser)
      ]
    <|> subcommand "open" "Open a note with your configured editor" (pure (OpenNote [] "temp"))

argParser :: Parser Command
argParser =
  Command
    <$> cmdsParser
    <*> optsParser

-- # Main Func

mdh :: (MonadIO io) => Config -> MdhCommands -> Opts -> io ()
mdh mCfg mCmd mOpts = do
  case mCmd of
    ShowTree -> do
      mt <- getTree mCfg
      case mt of
        Just t -> mdhLog mOpts "Collection tree loaded successfully" >> stdout (strTreeToShell t)
        Nothing -> mdhError "Collection tree loaded incorrectly"
    QuickWork -> do
      absDir <- absoluteMdhDir mCfg <&> (</> "work")
      mktree absDir
      let absFile = absDir </> "daily" <.> "md"
      touch absFile
      procs (fromString $ editor mCfg) [fromString absFile] empty
    QuickPersonal -> do
      absDir <- absoluteMdhDir mCfg <&> (</> "personal")
      mktree absDir
      let absFile = absDir </> "daily" <.> "md"
      touch absFile
      procs (fromString $ editor mCfg) [fromString absFile] empty
    ShowNotes ns -> do
      mt <- getTree mCfg
      let mpt = mt >>= nodeSearch ns
      case mpt of
        Just (p, _) -> stdout $ mdsAtRelDir mCfg mOpts p
        Nothing -> mdhDie "Couldn't find collection"
    _ -> mdhDie "not implemented"

  return ()

main :: IO ()
main = do
  (Command mCmd mOpts) <- options "Medahat, markdown notes utility for handdara" argParser
  mdhLog mOpts "Not currently using 'openLine' config option in this version of mdh"

  -- Attempt to get config
  cfgAttempt <- getConfig mOpts
  case cfgAttempt of
    Nothing -> mdhDie "Couldn't load config file, check ~/.config/mdh/"
    Just mCfg -> do
      mdhLog mOpts "Config loaded successfully, running"
      mdh mCfg mCmd mOpts
