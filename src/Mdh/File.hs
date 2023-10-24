module Mdh.File
  ( readToShell,
    readToNumberedShell,
    mdhTouch,
  )
where

import Turtle
import Prelude hiding (FilePath)

-- # File Reading/ Parsing

readToShell :: FilePath -> Shell Line
readToShell = input

readToNumberedShell :: FilePath -> Shell (Integer, Line)
readToNumberedShell = nl . input

-- Later, we may want to use managed resources or handles, but probably not right now
-- readHandle ::(MonadManaged mgd) => FilePath -> mgd Handle
-- readHandle :: FilePath -> Managed Handle
-- readHandle = readonly

-- # File Editing

-- | Special touch function that drops a small test string into instead of just creating it empty. 
-- This is because some editors (including neovim) have issues opening completely empty files
mdhTouch :: MonadIO io => FilePath -> io ()
mdhTouch file = do
  file'exists <- testfile file
  unless file'exists $ do
    output file (return "MDH NEW")
