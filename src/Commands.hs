module Commands (deploy, dockerCompose, initialize, release, Args, ProjectName, Tag) where

import Config (Config (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem (isFile)
import Filesystem.Path.CurrentOS (encodeString, fromText)
import System.Exit (exitFailure)

deploy :: Config -> IO ()
deploy config = return ()

type Args = [T.Text]

dockerCompose :: Config -> Args -> IO ()
dockerCompose config args = return ()

type ProjectName = T.Text

type ConfigPath = T.Text

initialize :: ProjectName -> ConfigPath -> IO ()
initialize projectName configPathTxt = do
  let configPath = fromText configPathTxt
      config = Config {projectName = projectName, port = "3000", tag = "v1.0,0", environments = M.empty}
      json = A.encodePretty config
  configFileExists <- isFile configPath
  if configFileExists
    then T.putStrLn ("'" <> configPathTxt <> "' already exists.") >> exitFailure
    else B.writeFile (encodeString configPath) json

type Tag = T.Text

release :: Config -> Tag -> IO ()
release config tag = return ()