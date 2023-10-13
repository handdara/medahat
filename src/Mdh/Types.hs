{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Mdh.Types
  ( Opts (Opts, verbose),
    MdhCommands (ShowTree, QuickWork, QuickPersonal, ShowNotes),
    Command (Command, cmd, opts),
    MPath,
    MdhTree (MNode, label, children),
  )
where

-- | Stores options set from command line arguments
newtype Opts = Opts
  { verbose :: Bool
  }

-- | Data type that represents possible `mdh` commands
-- and required arguments for those commands
data MdhCommands
  = ShowTree
  | QuickWork
  | QuickPersonal
  | ShowNotes MPath
  deriving (Show)

-- COMMANDS TO IMPL
-- \| MakeNode (FilePath, Text)
-- \| OpenNote MPath

-- | Packages 'Opts' and 'MdhComands' together
data Command = Command
  { cmd :: MdhCommands,
    opts :: Opts
  }

type MPath = [FilePath]

data MdhTree a = MNode {label :: a, children :: [MdhTree a]}
