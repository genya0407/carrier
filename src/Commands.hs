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
import NeatInterpolation
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
  dockerComposeYmlExists <- isFile (fromText "docker-compose.yml")
  if dockerComposeYmlExists
    then T.putStrLn "docker-compose.yml already exists" >> exitFailure
    else T.writeFile "docker-compose.yml" (initialDockerComposeYml config)

type Tag = T.Text

release :: Config -> Tag -> ConfigPath -> IO ()
release config tag configPath = do
  let updatedConfig = config {tag = tag}
      json = A.encodePretty updatedConfig
  B.writeFile (toString configPath) json
  forM_ (M.toList $ images config) $ \(imageName, dockerfile) -> do
    let imageNameWithTag = fullQualifiedImageName config imageName <> ":" <> tag
    callProcessWithEnv M.empty "docker" ["build", ".", "-f", dockerfile, "-t", imageNameWithTag]
    callProcessWithEnv M.empty "docker" ["push", imageNameWithTag]

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

fullQualifiedImageName config imageName = registry config <> "/" <> projectName config <> "_" <> imageName

initialDockerComposeYml config =
  let _imageName = fullQualifiedImageName config "web"
      _proj = projectName config
   in [text|
    version: '3'
    services:
      web:
        image: $_imageName:$${TAG}
        command: TODO
        ports:
          - "127.0.0.1:$${PORT:?err}:3000"
        restart: always
        links:
          - postgres
        env_file: .env
        depends_on:
          - postgres
      postgres:
        image: postgres:12-alpine
        restart: always
        env_file: .env
        volumes:
          - ${_proj}_pg_data:/var/lib/postgresql/data 

    volumes:
      ${_proj}_pg_data:
        external: true
  |]