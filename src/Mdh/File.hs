module Mdh.File
  ( readToShell,
    readToNumberedShell,
  )
where

import Prelude hiding (FilePath)

import Turtle

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

-- # File Editing
