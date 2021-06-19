module Commands (deploy, dockerCompose, initialize, release, Args, ProjectName, Tag) where

import Config (Config (..))
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import Data.Bifunctor
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.String.ToString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem (isFile)
import Filesystem.Path.CurrentOS (encodeString, fromText)
import System.Environment
import System.Exit
  ( ExitCode (ExitFailure, ExitSuccess),
    exitFailure,
  )
import System.Process
import System.Process
  ( CreateProcess (delegate_ctlc, env),
    createProcess,
    proc,
    waitForProcess,
  )

type ProjectName = T.Text

type ConfigPath = T.Text

type Context = T.Text

type Registry = T.Text

type Args = [T.Text]

deploy :: Config -> IO ()
deploy config = do
  dockerCompose config ["stop"]
  dockerCompose config ["up", "-d", "--build", "--force-recreate"]

dockerCompose :: Config -> Args -> IO ()
dockerCompose config args = do
  let allArgs = ["--context", context config] <> args
  callProcessWithEnv (allEnvs config) "docker-compose" (map T.unpack allArgs)
  return ()

initialize :: ProjectName -> ConfigPath -> Context -> Registry -> IO ()
initialize projectName configPathTxt context registry = do
  let configPath = fromText configPathTxt
      config =
        Config
          { projectName = projectName,
            port = "3000",
            tag = "v1.0,0",
            context = context,
            environments = M.empty,
            images = M.fromList [("web", "Dockerfile")],
            registry = registry
          }
      json = A.encodePretty config
  configFileExists <- isFile configPath
  if configFileExists
    then T.putStrLn ("'" <> configPathTxt <> "' already exists.") >> exitFailure
    else B.writeFile (encodeString configPath) json

type Tag = T.Text

release :: Config -> Tag -> ConfigPath -> IO ()
release config tag configPath = do
  let updatedConfig = config {tag = tag}
      json = A.encodePretty updatedConfig
  B.writeFile (toString configPath) json
  forM_ (M.toList $ images config) $ \(imageName, dockerfile) -> do
    let fullQualifiedImageName = registry config <> "/" <> projectName config <> "_" <> imageName <> ":" <> tag
    callProcessWithEnv M.empty "docker" ["build", ".", "-f", dockerfile, "-t", fullQualifiedImageName]
    callProcessWithEnv M.empty "docker" ["push", fullQualifiedImageName]

-- private functions

allEnvs config = M.fromList [("PORT", port config), ("TAG", tag config)] `M.union` environments config

callProcessWithEnv :: ToString s => M.Map T.Text T.Text -> FilePath -> [s] -> IO ()
callProcessWithEnv envs cmd args = do
  outerEnvList <- getEnvironment
  let p = (proc cmd (map toString args)) {delegate_ctlc = True, env = Just (outerEnvList <> (map (bimap T.unpack T.unpack) $ M.toList envs))}
  (_, _, _, handle) <- createProcess p
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure r -> exitFailure
