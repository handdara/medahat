module Mdh
  ( module Mdh.Config,
    module Mdh.Utils,
    module Mdh.Types,
    textFilesAtRelDir,
    openMd,
    editFile,
    openNoteCmd,
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
makeRelMdhDirAbsolute :: (MonadIO io) => Config -> FilePath -> io FilePath
makeRelMdhDirAbsolute mCfg rp = do
  mDir <- absoluteMdhDir mCfg
  return $ mDir </> dropFirst rp
  where
    dropFirst = foldr (</>) [] . tail . splitDirectories

-- | Make Shell listing all markdown (plaintext) files at a relative directory
textFilesAtRelDir :: Config -> Opts -> FilePath -> Shell Line
textFilesAtRelDir mCfg mOpts p = do
  absReqDir <- makeRelMdhDirAbsolute mCfg p
  dir <- ls absReqDir
  status <- stat dir
  mdhLog mOpts $ "checking " <> fromString dir
  if not (isDirectory status) && validTextExtension dir
    then (return . unsafeTextToLine . fromString) dir
    else empty

-- | Make Shell listing all markdown (plaintext) files at a relative directory AND subdirectories
textFilesAtRelDir'tree :: Config -> Opts -> FilePath -> Shell Line
textFilesAtRelDir'tree mCfg mOpts p = do
  absReqDir <- makeRelMdhDirAbsolute mCfg p
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
editFileLine :: (MonadIO io, Integral n) => Config -> FilePath -> n -> io ()
editFileLine = undefined

openMdFilter :: FilePath -> Shell Line -> Shell FilePath
openMdFilter reqName' ps = do
  p <- unpack . lineToText <$> ps
  let (name'requested, mExt'requested) = splitExtension reqName'
  let (name'testing, mExt'testing) = splitExtension p
  if filename name'testing /= name'requested
    then empty
    else case mExt'requested of
      -- no extension given by user
      Nothing -> return p
      mre -> if mre == mExt'testing then return p else empty

-- | Open the first acceptable markdown file found
-- In this context, acceptable means that the extension requested/found is valid a text file type,
-- the files have the same name, and if an extension is given, the same extension
openMd :: (MonadIO io) => Config -> Opts -> FilePath -> FilePath -> io ()
openMd mCfg mOpts path name = do
  unless
    (validTextExtensionOrNone name)
    (mdhDie $ "Not an allowed file extension: " <> repr (extension name))
  let possibleFiles = openMdFilter name $ textFilesAtRelDir'tree mCfg mOpts path
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

openNoteCmd :: MonadIO m => Config -> Opts -> MPath -> m ()
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
    Just (p'rel, _) -> openMd mCfg mOpts p'rel name
