{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Mdh
import Turtle
import Prelude hiding (FilePath)

-- # Argument Parsing

nodesParser :: Parser FilePath
nodesParser = argPath "nodes" "successive sub-collections to search down collection tree"

showTreeParser :: Parser MdhCommands
showTreeParser = ShowTree <$> many nodesParser

showNotesParser :: Parser MdhCommands
showNotesParser = ShowNotes <$> some nodesParser

openNoteParser :: Parser MdhCommands
openNoteParser =
  OpenNote
    <$> some (argPath "[nodes] note" "NOTE: note to open, if no extension is given a default `.md` is applied. NODES: successive sub-collections to search down collection tree")

mkNodeParser :: Parser MdhCommands
mkNodeParser =
  MakeNode
    <$> many nodesParser
    <*> optPath "name" 'n' "name for the new collection"

mkNoteParser :: Parser MdhCommands
mkNoteParser =
  MakeNote
    <$> many nodesParser
    <*> optPath "name" 'n' "name for the new note"
    <*> switch "touch-only" 't' "By default medahat will open the file in the configured editor, this flag turns that off"

cmdsParser :: Parser MdhCommands
cmdsParser =
  subcommandGroup
    "Opening notes:"
    [ ("qw", "Quick open daily work notes", pure QuickWork),
      ("qp", "Quick open daily work notes", pure QuickPersonal),
      ("open", "Open a note with your configured editor", openNoteParser),
      ("o", "Alias for `open`", openNoteParser)
    ]
    <|> subcommandGroup
      "Show commands:"
      [ ("tree", "*Default Command*: Show collection tree,\n  (collections are basically synonymous with directories)", showTreeParser),
        ("show", "Find and show notes at a collection or sub-collection", showNotesParser),
        ("s", "Alias for `show`", showNotesParser)
      ]
    <|> subcommandGroup
      "Make new notes and collections:"
      [ ("mc", "Make new (sub)collection at a given node in the collection tree", mkNodeParser),
        ("mn", "Make new note at a given node in the collection tree", mkNoteParser)
      ]

optsParser :: Parser Opts
optsParser =
  Opts
    <$> switch "verbose" 'v' "Verbose mode allows logs"
    <*> optional (optPath "config" 'c' "Provide config file")

argParser :: Parser Command
argParser =
  Command
    <$> optional cmdsParser
    <*> optsParser

-- # Main Funcs

-- | This is handle most of the execution logic, basically the entire program minus parsing configs/options
mdh :: (MonadIO io) => Config -> MdhCommands -> Opts -> io ()
mdh mCfg mCmd mOpts = do
  case mCmd of
    ShowTree [] -> do
      mt <- getTree mCfg
      case mt of
        Nothing -> mdhError "Collection tree loaded incorrectly"
        Just t ->  do
          mdhLog mOpts "Collection tree loaded successfully"
          stdout (strTreeToShell t)
    ShowTree ns -> do
      mt <- getTree mCfg
      let mpt = mt >>= nodeSearch ns
      case mpt of
        Nothing -> mdhError "Collection tree loaded incorrectly"
        Just (_, subTree) -> do
          mdhLog mOpts "Collection tree loaded successfully"
          stdout (strTreeToShell subTree)
    QuickWork -> do
      absDir <- absoluteMdhDir mCfg <&> (</> "work")
      mktree absDir
      let absFile = absDir </> "daily" <.> "md"
      touch absFile
      editFile mCfg absFile
    QuickPersonal -> do
      absDir <- absoluteMdhDir mCfg <&> (</> "personal")
      mktree absDir
      let absFile = absDir </> "daily" <.> "md"
      touch absFile
      editFile mCfg absFile
    ShowNotes ns -> do
      mt <- getTree mCfg
      let mpt = mt >>= nodeSearch ns
      case mpt of
        Just (p, _) -> stdout $ textFilesAtRelDir mCfg mOpts p
        Nothing -> mdhDie "Couldn't find collection"
    OpenNote ns -> 
      openNoteCmd mCfg mOpts ns
    MakeNode ns new -> mkNode mCfg mOpts ns new
    MakeNote ns new tFlag -> mkNote mCfg mOpts ns new tFlag
    -- _ -> mdhDie $ "command not yet implemented: " <> (head . split (== ' ') . repr $ mCmd)

-- | Main entrypoint
main :: IO ()
main = do
  (Command maybeMedCmd mOpts) <- options "Medahat, markdown notes utility for handdara" argParser

  -- Attempt to get config
  cfgAttempt <- getConfig mOpts
  case (cfgAttempt, maybeMedCmd) of
    (Nothing, _) -> mdhDie "Couldn't load config file, check ~/.config/mdh/"
    (Just mCfg, Nothing) -> do
      mdhLog mOpts "Config loaded successfully, running"
      mdh mCfg (ShowTree []) mOpts
    (Just mCfg, Just medCmd) -> do
      mdhLog mOpts "Config loaded successfully, running"
      mdh mCfg medCmd mOpts
