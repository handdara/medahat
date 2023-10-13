{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mdh.Utils (
  getTree,
  strTreeToShell,
  absoluteMdhDir,
  mdhLog,
  mdhWarn,
  mdhError,
  mdhDie,
  nodeSearch,
) where

import qualified Control.Foldl as F
import qualified Data.Bifunctor as BF
import Mdh.Types
import Mdh.Config
import Turtle

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

-- ## Conversions

-- | pathToNodes just splits on '/'
-- >>> pathToNodes "/home/name/mdhtests"
pathToNodes :: FilePath -> MPath
pathToNodes p =
  [ filter (`notElem` ("/\\" :: FilePath)) n
    | n <- splitDirectories p,
      n /= "/"
  ]

nodeSearch :: MPath -> MdhTree FilePath -> Maybe (FilePath, MdhTree FilePath)
nodeSearch [ ] _ = mdhError "nodeSearch called with empty list"
nodeSearch [n] t
  | label t == n = Just (n, t)
  | null mps = Nothing
  | otherwise = BF.first (label t </>) <$> head mps
  where
    mps' = nodeSearch [n] <$> children t
    mps = dropWhile (== Nothing) mps'
nodeSearch (n : ns) t =
  do
    pt <- nodeSearch [n] t
    pts <- nodeSearch ns (snd pt)
    return (BF.first ((fst pt </>) . dropFirst) pts)
  where
    dropFirst = foldr1 (</>) . tail . splitDirectories

nodeListToTree [x] = MNode x []
nodeListToTree (x : xs) = MNode x [nodeListToTree xs]

pathToTree = nodeListToTree . pathToNodes

strTreeToShell :: MdhTree String -> Shell Line
strTreeToShell = select . strTreeToLines' 0
  where
    strTreeToLines' n (MNode l cs) =
      (unsafeTextToLine . fromString) (replicate (2 * n) ' ' ++ l)
        : (cs >>= strTreeToLines' (n + 1))

-- # Logging Utilities

mdhLog :: (MonadIO io) => Opts -> Shell Line -> io ()
mdhLog opts msg = when (verbose opts) (stdout $ "LOG: " <> msg)

mdhWarn :: (MonadIO io) => Shell Line -> io ()
mdhWarn = stderr . ("WARNING: " <>)

mdhDie :: (MonadIO io) => Text -> io ()
mdhDie = die . ("ERROR: " <>)

mdhError :: String -> a
mdhError = error . ("ERROR: " <>)

-- # File/Directory Management

formatRelPath :: (MonadIO io) => FilePath -> io FilePath
formatRelPath d =
  if head (head sds) == '~'
    then (</> foldl (</>) mempty (tail sds)) <$> home
    else error "MdhUtils.hs:132: fix this spot (which error?)"
  where
    sds = splitDirectories d

absoluteMdhDir :: MonadIO io => Config -> io FilePath
absoluteMdhDir cfg =
  if isAbsolute d
    then return d
    else formatRelPath d
  where
    d = mdhDir cfg

dirListFrCfg :: Config -> Shell FilePath
dirListFrCfg c =
  do
    t <- dirListFrCfg' c
    s <- stat t
    if isDirectory s && '.' `notElem` t
      then return t
      else empty
  where
    dirListFrCfg' = lstree <=< absoluteMdhDir

getFullDirTree :: (MonadIO io) => Config -> io (MdhTree FilePath)
getFullDirTree c =
  do
    let ts = pathToTree <$> dirListFrCfg c
    tsl <- fold ts F.list
    return (foldr1 (<>) tsl)

-- Mostly a debug func, dumps the list of subdirectories at mdhDir
-- Used to develop the code to get the directory tree
dumpDirTree :: (MonadIO io) => Config -> io ()
dumpDirTree = stdout . (strTreeToShell <=< getFullDirTree)

getRootNodeName :: Config -> FilePath
getRootNodeName = last . splitDirectories . mdhDir

-- | This will validate a parsed file tree and then cut off the paths
-- leading up to the mdh home directory, this is based on the fact that there
-- should be no branching until after the home directory is reached in the tree
clipDirTree :: Config -> MdhTree FilePath -> Maybe (MdhTree FilePath)
clipDirTree c t
  | label t == getRootNodeName c = Just t
  | length (children t) == 1 = clipDirTree c (head . children $ t)
  | otherwise = Nothing

getTree :: (MonadIO io) => Config -> io (Maybe (MdhTree FilePath))
getTree c = clipDirTree c <$> getFullDirTree c
