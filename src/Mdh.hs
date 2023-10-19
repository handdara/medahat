module Mdh
  ( module Mdh.Config,
    module Mdh.Utils,
    module Mdh.Types,
    textFilesAtRelDir,
    editFile,
    openNoteCmd,
    mkNode,
    mkNote,
  )
where

import qualified Control.Foldl as F
import Data.Text (unpack)
import Mdh.Config
import Mdh.Types
import Mdh.Utils
import Turtle

-- # CLI Command Funcs

-- makeRelMdhDirAbsolute :: Config -> FilePath -> io FilePath
mkRelMdhDirAbsolute :: (MonadIO io) => Config -> FilePath -> io FilePath
mkRelMdhDirAbsolute mCfg rp = do
  mDir <- absoluteMdhDir mCfg
  return $ mDir </> dropFirst rp
  where
    dropFirst = foldr (</>) [] . tail . splitDirectories

-- | Make Shell listing all markdown (plaintext) files at a relative directory
textFilesAtRelDir :: Config -> Opts -> FilePath -> Shell Line
textFilesAtRelDir mCfg mOpts p = do
  absReqDir <- mkRelMdhDirAbsolute mCfg p
  dir <- ls absReqDir
  status <- stat dir
  mdhLog mOpts $ "checking " <> fromString dir
  if not (isDirectory status) && validTextExtension dir
    then (return . unsafeTextToLine . fromString) dir
    else empty

-- | Make Shell listing all markdown (plaintext) files at a relative directory AND subdirectories
textFilesAtRelDir'tree :: Config -> Opts -> FilePath -> Shell Line
textFilesAtRelDir'tree mCfg mOpts p = do
  absReqDir <- mkRelMdhDirAbsolute mCfg p
  dir <- lstree absReqDir
  status <- stat dir
  mdhLog mOpts $ "checking " <> fromString dir
  if not (isDirectory status) && validTextExtension dir
    then (return . unsafeTextToLine . fromString) dir
    else empty

-- | The function that actually opens the editor
editFile :: (MonadIO io) => Config -> FilePath -> io ()
editFile mCfg file = do
  previous <- pwd
  mdhPath <- absoluteMdhDir mCfg
  cd mdhPath
  procs (fromString $ editor mCfg) [fromString file] empty
  cd previous

-- | TODO: Variation of `editFile` that will open to a specific line
editFile'atLine :: (MonadIO io, Integral n) => Config -> FilePath -> n -> io ()
editFile'atLine = undefined

filterMatchingRequested :: FilePath -> Shell Line -> Shell FilePath
filterMatchingRequested reqName' ps = do
  p <- unpack . lineToText <$> ps
  let (name'requested, mExt'requested) = splitExtension reqName'
  let (name'testing, mExt'testing) = splitExtension p
  if filename name'testing /= name'requested
    then empty
    else case mExt'requested of
      -- no extension given by user
      Nothing -> return p
      mre -> if mre == mExt'testing then return p else empty

-- | Open the first acceptable text/markdown file found
-- In this context, acceptable means that the extension requested/found is valid a text file type,
-- the files have the same name, and if an extension is given, the same extension
findAndOpenTextFile :: (MonadIO io) => Config -> Opts -> FilePath -> FilePath -> io ()
findAndOpenTextFile mCfg mOpts path name = do
  unless
    (validTextExtensionOrNone name)
    (mdhDie $ "Not an allowed file extension: " <> repr (extension name))
  let possibleFiles = filterMatchingRequested name $ textFilesAtRelDir'tree mCfg mOpts path
  (numFilesFound, mp) <- fold possibleFiles ((,) <$> F.length <*> F.head) -- :: Shell (Int, Maybe FilePath)
  case mp of
    Nothing -> mdhDie "No files found"
    Just p -> do
      when (numFilesFound > 1) $ dumpFiles mOpts possibleFiles
      editFile mCfg p
  where
    dumpFiles mo fs = do
      mdhWarn mo "More than one acceptable file found, opening first"
      sh $ do
        pf <- fs
        mdhWarn mOpts $ "  Found: " <> fromString pf

openNoteCmd :: MonadIO io => Config -> Opts -> MPath -> io ()
openNoteCmd mCfg mOpts nodes'name = do
  let ns = init nodes'name
  let name = last nodes'name
  mt <- getTree mCfg
  let mpt = mt >>= nodeSearch ns
  case mpt of
    Nothing -> do
      sh $ do
        p <- select ns
        mdhWarn mOpts $ "  [NODES]: " <> fromString p
      mdhDie "Couldn't find collection"
    Just (p'rel, _) -> findAndOpenTextFile mCfg mOpts p'rel name

-- | Make a directory at a requested node (basically a glorified `mkdir`) note that this calls
-- `Turtle.mktree` internally, so a path of nodes can be given, not just 1, e.g. "multiple/nodes/given"
mkNode :: MonadIO io => Config -> Opts -> MPath -> FilePath -> io ()
mkNode mConf mOpts ns new = do
  mt <- getTree mConf
  let mpt = mt >>= nodeSearch ns
  case mpt of
    Nothing -> mdhDie "Couldn't find collection"
    Just (path'rel, _) -> do
      path'abs <- mkRelMdhDirAbsolute mConf path'rel
      let pathToMk = path'abs </> new
      mdhLog mOpts "Collection found, making directory:"
      mdhLog mOpts $ fromString ("  " <> pathToMk)
      mktree pathToMk

-- | Make a text file at a requested node. 
-- - Searches down subcollections
-- - Verifies valid extension type
-- - touches the file
-- - by default then edits, but this can be turned off
mkNote :: MonadIO io => Config -> Opts -> MPath -> FilePath -> Bool -> io ()
mkNote mConf mOpts ns newTxtFile touchFlag = do
  mt <- getTree mConf
  let mpt = mt >>= nodeSearch ns
  unless
    (validTextExtensionOrNone newTxtFile)
    (mdhDie $ "Not an allowed file extension: " <> repr (extension newTxtFile))
  case mpt of
    Nothing -> mdhDie "Couldn't find collection"
    Just (path'rel, _) -> do
      path'abs <- mkRelMdhDirAbsolute mConf path'rel
      mdhLog mOpts "Collection found at:"
      mdhLog mOpts $ fromString ("  " <> path'abs)
      let {
        toWrite = path'abs </> if hasExtension newTxtFile
          then newTxtFile
          else newTxtFile <.> "md"
      }
      touch toWrite
      unless touchFlag $ editFile mConf toWrite
