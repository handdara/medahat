
module Mdh.Types
  ( Opts (..),
    MdhCommands (..),
    Command (..),
    MPath,
    MdhTree (..),
    Config (..),
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
    openToLine :: Bool
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
  = ShowTree {nodes :: MPath}
  | QuickWork
  | QuickPersonal
  | ShowNotes MPath
  | OpenNote MPath
  | MakeNode {nodes :: MPath, newNode :: FilePath}
  | MakeNote {nodes :: MPath, newNote :: FilePath, edit :: Bool}
  deriving (Show)

-- | Packages 'Opts' and 'MdhCommands' together
data Command = Command
  { cmd :: Maybe MdhCommands,
    opts :: Opts
  }

type MPath = [FilePath]

data MdhTree a = MNode {label :: a, children :: [MdhTree a]}

-- # Typeclass Implementations

-- ## MdhTree
-- ### Show
showWithTabNumber :: (Show s) => Int -> MdhTree s -> String
showWithTabNumber n (MNode lab []) = replicate (2 * n) ' ' ++ show lab ++ "\n"
showWithTabNumber n (MNode lab cs) =
  replicate (2 * n) ' '
    ++ show lab
    ++ "\n"
    ++ foldr1 (++) childStrings
  where
    childStrings = map (showWithTabNumber (n + 1)) cs

instance (Show a) => Show (MdhTree a) where
  show = showWithTabNumber 0

-- ### Eq
instance (Eq a) => Eq (MdhTree a) where
  lab == r = (label lab == label r) && (children lab == children r)

-- ### Semigroup

-- Even if the left lable == right label, no children to add
-- if the left table /= the right label, then don't add anything
insert :: (Eq a, Monoid a) => MdhTree a -> MdhTree a -> MdhTree a
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
  lab <> r
    | lab `inMTree` r = insert lab {-into-} r
    | label r == mempty = MNode mempty (lab : children r)
    | label lab == mempty = MNode mempty (r : children lab)
    | otherwise = MNode mempty [lab, r]

inMTree :: (Eq a) => MdhTree a -> MdhTree a -> Bool
inMTree lab r
  | label lab == label r = True
  | otherwise = foldr (\c acc -> label lab == label c || acc) False (children r)

-- ## Config
-- ### Aeson typeclasses
instance ToJSON Config where
  toJSON (Config mDir mEditor b) =
    object ["mdhDir" .= mDir, "editor" .= mEditor, "openLine" .= b]

  toEncoding (Config mDir mEditor b) =
    pairs ("mdhDir" .= mDir <> "editor" .= mEditor <> "openLine" .= b)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
      <$> v
        .: "mdhDir"
      <*> v
        .: "editor"
      <*> v
        .: "openLine"
