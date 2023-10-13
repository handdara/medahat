module Mdh
  ( module Mdh.Config,
    module Mdh.Utils,
    module Mdh.Types,
    mdsAtRelDir,
  )
where

import Mdh.Config
import Mdh.Types
import Mdh.Utils

import Turtle

-- # CLI Command Funcs

-- | Make Shell listing all markdown files at a relative directory
mdsAtRelDir :: Config -> Opts -> FilePath -> Shell Line
mdsAtRelDir mCfg mOpts p = do
  absMdhDir <- absoluteMdhDir mCfg
  let absReqDir = absMdhDir </> dropFirst p
  dir <- ls absReqDir
  status <- stat dir
  mdhLog mOpts $ "checking " <> fromString dir
  if not (isDirectory status) && (extension dir == Just "md")
    then return $ (unsafeTextToLine . getLast) dir
    else empty
  where
    dropFirst = foldr (</>) [] . tail . splitDirectories
    getLast = fromString . last . splitDirectories
