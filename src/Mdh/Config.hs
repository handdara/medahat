{-# LANGUAGE OverloadedStrings #-}

module Mdh.Config
  ( Config (Config, mdhDir, editor, openLine),
    getConfig,
  )
where

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as LIO
import Turtle

-- --- Config Type ---

data Config = Config
  { mdhDir :: FilePath,
    editor :: String,
    openLine :: Bool
  }
  deriving (Show)

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

defaultConfig = Config ("~" </> "mdh") "hx" True

-- --- ---

relConfigDir = ".config" </> "mdh"

configDir = do
  hd <- home
  return $ hd </> relConfigDir

-- Following func assumes cfg dir has been make already
-- It writes a new default config and returns that default config
mkNewConfig :: FilePath -> IO (Maybe Config)
mkNewConfig cfp = do
  let dc = defaultConfig
  LIO.writeFile cfp $ encodeToLazyText dc
  return (Just dc)

-- Need to read the config file, make it into text/bytestring,
-- and then pass it to aeson for decoding
loadConfig :: FilePath -> IO (Maybe Config)
loadConfig cfp = do
  raw <- B.readFile cfp
  return (decode raw)

-- | Does the cfg directory exist? if not this func makes it
-- Does the cfg file      exist? if not this func makes it
-- We could also add more complicated search functionality, ie loading any
-- json file in the cfg dir
getConfig :: IO (Maybe Config)
getConfig = do
  cfgDir <- configDir
  let cfgFilePath = cfgDir </> "config.json"

  cfgDirExists <- testdir cfgDir
  if not cfgDirExists
    then mktree cfgDir >> mkNewConfig cfgFilePath
    else do
      cfgExists <- testfile cfgFilePath
      if cfgExists
        then loadConfig cfgFilePath
        else mkNewConfig cfgFilePath
