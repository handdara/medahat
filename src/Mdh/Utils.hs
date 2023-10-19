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
    textExtensions,
    validTextExtension,
    validTextExtensionOrNone,
    dayMonth,
  )
where

import qualified Control.Foldl as F
import qualified Data.Bifunctor as BF
import Mdh.Types
import Turtle
import Data.Text (unpack, split)

-- | nodeSearch searches for successive nodes, that is, it finds the first node in a list,
-- then searches from that nodes for the next, etc.
-- If it doesn't find the node or is called with and empty list it returns `Nothing` (failure)
nodeSearch :: MPath -> MdhTree FilePath -> Maybe (FilePath, MdhTree FilePath)
nodeSearch [] t = Just (label t, t)
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

-- ## Conversions

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

mdhLog :: (MonadIO io) => Opts -> Text -> io ()
mdhLog opts = when (verbose opts) . stdout . return . unsafeTextToLine . ("LOG: " <>) 

mdhWarn :: (MonadIO io) => Opts -> Text -> io ()
mdhWarn _ = stderr . return . unsafeTextToLine . ("WARNING: " <>) 

mdhDie :: (MonadIO io) => Text -> io a
mdhDie = die . ("KILLED: " <>) 

mdhError :: Text -> anything
mdhError = error . unpack . ("ERROR: " <>)

-- # File/Directory Management

textExtensions :: [String]
textExtensions = ["md", "text", "txt"]

-- | is an extension in the list of allowable text file extension
validTextExtension :: FilePath -> Bool
validTextExtension = (`elem` map Just textExtensions) . extension

validTextExtensionOrNone :: FilePath -> Bool
validTextExtensionOrNone = (`elem` (Nothing : map Just textExtensions)) . extension

-- | take a relative path and format it to absolute
formatRelPath :: (MonadIO io) => FilePath -> io FilePath
formatRelPath d =
  case head d of
    '~' -> home <&> (</> dropFirst d)
    '.' -> pwd <&> (</> dropFirst d)
    _ -> pwd <&> (</> d)
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

-- # Time and Dates

utcTimeToDayAndMonth :: UTCTime -> Maybe (Text, Text)
utcTimeToDayAndMonth t = 
  if length ts /= 3
    then Nothing
    else do
      let m = ts !! 1
          d = ts !! 2 
      mNum <- toMonthText $ unpack m
      return (d, mNum)
  where 
    ymd'other = head . split (==' ') . (fromString . show) :: UTCTime -> Text
    year'month'day = split (=='-') . ymd'other 
    ts = year'month'day t
    months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
    toMonthText mt = do
      let mi = read mt - 1:: Int
      if 0 <= mi && mi <= 11 
        then Just (months !! mi)
        else Nothing

-- | Attempt to get the current day and month of system as text
-- example success might return `Just ("19","jan")`
dayMonth :: (MonadIO io) => io (Maybe (Text, Text))
dayMonth = utcTimeToDayAndMonth <$> date
    

