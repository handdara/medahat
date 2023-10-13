{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mdh.Types
  ( Opts (Opts, verbose, cfgDir),
    MdhCommands
      ( ShowTree,
        QuickWork,
        QuickPersonal,
        ShowNotes,
        OpenNote,
        MakeNode,
        nodes,
        newNode,
        MakeNote,
        newNote
      ),
    Command (Command, cmd, opts),
    MPath,
    MdhTree (MNode, label, children),
    Config (Config, mdhDir, editor, openLine),
  )
where

import Data.Aeson
    ( FromJSON(parseJSON),
      ToJSON(toJSON, toEncoding),
      pairs,
      (.:),
      withObject,
      object,
      KeyValue((.=)) )
import Turtle
import Prelude hiding (FilePath)

-- | Config type for storing medahat configurations
data Config = Config
  { mdhDir :: FilePath,
    editor :: String,
    openLine :: Bool
  }
  deriving (Show)

-- | Stores options set from command line arguments
data Opts = Opts
  { verbose :: Bool,
    cfgDir :: Maybe FilePath
  }
  deriving (Show)

-- | Data type that represents possible `mdh` commands
-- and required arguments for those commands
data MdhCommands
  = ShowTree
  | QuickWork
  | QuickPersonal
  | ShowNotes MPath
  | OpenNote MPath
  | MakeNode {nodes :: MPath, newNode :: FilePath}
  | MakeNote {nodes :: MPath, newNote :: FilePath}
  deriving (Show)

-- | Packages 'Opts' and 'MdhComands' together
data Command = Command
  { cmd :: MdhCommands,
    opts :: Opts
  }

type MPath = [FilePath]

data MdhTree a = MNode {label :: a, children :: [MdhTree a]}

-- # Typeclass Implementations

-- ## MdhTree
-- ### Show
showWithTabNumber :: (Show s) => Int -> MdhTree s -> String
showWithTabNumber n (MNode l []) = replicate (2 * n) ' ' ++ show l ++ "\n"
showWithTabNumber n (MNode l cs) =
  replicate (2 * n) ' '
    ++ show l
    ++ "\n"
    ++ foldr1 (++) childStrings
  where
    childStrings = map (showWithTabNumber (n + 1)) cs

instance (Show a) => Show (MdhTree a) where
  show = showWithTabNumber 0

-- ### Eq
instance (Eq a) => Eq (MdhTree a) where
  l == r = (label l == label r) && (children l == children r)

-- ### Semigroup
insert :: (Eq a, Monoid a) => MdhTree a -> MdhTree a -> MdhTree a
-- Even if the left lable == right label, no children to add
-- if the left table /= the right label, then don't add anything
insert (MNode _ []) r = r
insert (MNode ll lcs) (MNode rl []) =
  if ll == rl
    then MNode rl lcs
    else MNode rl []
insert (MNode ll [lc]) (MNode rl rcs)
  | ll /= rl = MNode rl (insert leftTree `map` rcs)
  | label lc `elem` map label rcs = MNode rl (insert lc `map` rcs)
  | otherwise = MNode rl (lc : rcs)
  where
    leftTree = MNode ll [lc]
insert (MNode ll (lc : lcs)) r =
  insert lsingle (insert lmult r)
  where
    lsingle = MNode ll [lc]
    lmult = MNode ll lcs

instance (Eq a, Monoid a) => Semigroup (MdhTree a) where
  l <> r
    | l `inMTree` r = insert l {-into-} r
    | label r == mempty = MNode mempty (l : children r)
    | label l == mempty = MNode mempty (r : children l)
    | otherwise = MNode mempty [l, r]

inMTree :: (Eq a) => MdhTree a -> MdhTree a -> Bool
inMTree l r
  | label l == label r = True
  | otherwise = foldr (\c acc -> label l == label c || acc) False (children r)

-- ## Config
-- ### Aeson typeclasses
instance ToJSON Config where
  toJSON (Config d e b) =
    object ["mdhDir" .= d, "editor" .= e, "openLine" .= b]

  toEncoding (Config d e b) =
    pairs ("mdhDir" .= d <> "editor" .= e <> "openLine" .= b)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config
      <$> v
        .: "mdhDir"
      <*> v
        .: "editor"
      <*> v
        .: "openLine"
