{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mdh.Utils
  ( getTree,
    strTreeToShell,
    absoluteMdhDir,
    mdhLog,
    mdhWarn,
    mdhError,
    mdhDie,
    nodeSearch,
    formatRelPath,
  )
where

import qualified Control.Foldl as F
import qualified Data.Bifunctor as BF
import Mdh.Types
import Turtle

-- ## Conversions

nodeSearch :: MPath -> MdhTree FilePath -> Maybe (FilePath, MdhTree FilePath)
nodeSearch [] _ = mdhError "nodeSearch called with empty list"
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

nodeListToTree :: [a] -> MdhTree a
nodeListToTree [] = mdhError "can't call nodeListToTree with empty list"
nodeListToTree [x] = MNode x []
nodeListToTree (x : xs) = MNode x [nodeListToTree xs]

-- | pathToNodes just splits on '/'
-- >>> pathToNodes "/home/name/mdhtests"
pathToNodes :: FilePath -> MPath
pathToNodes p =
  [ filter (`notElem` ("/\\" :: FilePath)) n
    | n <- splitDirectories p,
      n /= "/"
  ]

-- | This takes a single directory path, taken literally, returns the
-- the equivalent tree, where each node only should have a single child
-- - the path is taken literally, i.e. "~/example" yields  "~" --> "example"
pathToTree :: FilePath -> MdhTree FilePath
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

mdhWarn :: (MonadIO io) => Opts -> Shell Line -> io ()
mdhWarn _ = stderr . ("WARNING: " <>)

mdhDie :: (MonadIO io) => Text -> io ()
mdhDie = die . ("KILLED: " <>)

mdhError :: String -> a
mdhError = error . ("ERROR: " <>)

-- # File/Directory Management

-- | take a relative path and format it to absolute
formatRelPath :: (MonadIO io) => FilePath -> io FilePath
formatRelPath d =
  case head d of
    '~' -> home <&> (</> dropFirst d)
    '.' -> pwd  <&> (</> dropFirst d)
    _   -> pwd  <&> (</>           d)
  where
    dropFirst = foldl (</>) mempty . tail . splitDirectories

absoluteMdhDir :: (MonadIO io) => Config -> io FilePath
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
