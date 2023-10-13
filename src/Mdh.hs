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

mdsAtRelDir :: Config -> [Char] -> Shell Line
mdsAtRelDir c p = do
  absMdhDir <- absoluteMdhDir c
  let absReqDir = absMdhDir </> dropFirst p
  dir <- ls absReqDir
  status <- stat dir
  if not (isDirectory status) && (extension dir == Just "md")
    then return $ (unsafeTextToLine . getLast) dir
    else empty
  where
    dropFirst = foldr (</>) [] . tail . splitDirectories
    getLast = fromString . last . splitDirectories
