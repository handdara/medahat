module Mdh.Config (getConfig, defaultConfig) where

import Data.Aeson (decode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as LIO
import Mdh.Types
import Mdh.Utils (formatRelPath, mdhWarn)
import Turtle

defaultConfig :: Config
defaultConfig = Config ("~" </> "Documents" </> "medahat") "hx" False

relConfigDir :: FilePath
relConfigDir = ".config" </> "medahat"

getDefaultConfigDirectory :: IO FilePath
getDefaultConfigDirectory = do
  hd <- home
  return $ hd </> relConfigDir

-- Following func assumes cfg dir has been make already
-- It writes a new default config and returns that default config
mkNewConfig :: FilePath -> IO (Maybe Config)
mkNewConfig cfp = do
  let dc = defaultConfig
  LIO.writeFile cfp $ encodeToLazyText dc
  return (Just dc)

-- | Reads the config file, makes it into a text/bytestring,
-- and then passes it to aeson for decoding
loadConfig :: FilePath -> IO (Maybe Config)
loadConfig cfp = do
  raw <- B.readFile cfp
  return (decode raw)

formatCfgPath :: (MonadIO io) => FilePath -> io FilePath
formatCfgPath dir =
  if isAbsolute dir
    then return dir
    else formatRelPath dir

getConfig' :: Opts -> FilePath -> IO (Maybe Config)
getConfig' mOpts cfgFilePath = do
  cfgFileExists <- testfile cfgFilePath
  if cfgFileExists
    then loadConfig cfgFilePath
    else do
      mdhWarn mOpts "config file doesn't exist, running with default"
      return (Just defaultConfig)

-- | Does the cfg directory exist? if not this func makes it
-- Does the cfg file exist? if not this func makes it
-- Returns a configuration i
getConfig :: (MonadIO io) => Opts -> io (Maybe Config)
getConfig mOpts = liftIO $
  case cfgDir mOpts of
    Nothing -> do
      defCfgDir <- getDefaultConfigDirectory
      let cfgFilePath = defCfgDir </> "config.json"
      cfgDirExists <- testdir defCfgDir
      if cfgDirExists
        then getConfig' mOpts cfgFilePath
        else do
          mdhWarn mOpts "config directory doesn't exist (~/.config/medahat), making new config"
          mktree defCfgDir
          mkNewConfig cfgFilePath
    Just cfgFilePath -> (getConfig' mOpts <=< formatCfgPath) cfgFilePath

-- We could also add more complicated search functionality, ie loading any
-- json file in the cfg dir, maybe a todo
-- TODO: also would be nice to allow setting config fields or dumping new config via command line
