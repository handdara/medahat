module Mdh
  ( module Mdh.Config,
    module Mdh.Utils,
    module Mdh.Types,
    mdsAtRelDir,
    openMd,
  )
where

import Mdh.Config
import Mdh.Types
import Mdh.Utils

import Turtle

-- # CLI Command Funcs

-- makeRelMdhDirAbsolute :: Config -> FilePath -> io FilePath
makeRelMdhDirAbsolute :: MonadIO io => Config -> FilePath -> io FilePath
makeRelMdhDirAbsolute mCfg rp = do
  mDir <- absoluteMdhDir mCfg
  return $ mDir </> dropFirst rp
  where
    dropFirst = foldr (</>) [] . tail . splitDirectories

-- | Make Shell listing all markdown files at a relative directory
mdsAtRelDir :: Config -> Opts -> FilePath -> Shell Line
mdsAtRelDir mCfg mOpts p = do
  absReqDir <- makeRelMdhDirAbsolute mCfg p
  dir <- ls absReqDir
  status <- stat dir
  mdhLog mOpts $ "checking " <> fromString dir
  if not (isDirectory status) && (extension dir == Just "md")
    then return $ (unsafeTextToLine . getLast) dir
    else empty
  where
    getLast = fromString . last . splitDirectories

openMd :: (MonadIO io) => Opts -> Config -> FilePath -> FilePath -> io ()
openMd mOpts mCfg path name' = do
  let name = if hasExtension name' then name' else name' <.> "md"
  fullPath <- makeRelMdhDirAbsolute mCfg path <&> (</> name)
  mdExists <- testfile fullPath
  unless mdExists $ do
    -- currently we are just making the file and opening, but this could also cause mdh to die
    mdhWarn mOpts $ "Did not find note at requested collection, created before opening. File can be found at: " <> fullPath
    touch fullPath
  procs (fromString $ editor mCfg) [fromString fullPath] empty
