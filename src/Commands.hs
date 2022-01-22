module Commands (deploy, dockerCompose, initialize, release, Args, ProjectName, Tag) where

import Config (Config (..))
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.String.ToString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Filesystem (isFile)
import Filesystem.Path.CurrentOS (encodeString, fromText)
import NeatInterpolation
import System.Environment
import System.Exit
  ( ExitCode (ExitFailure, ExitSuccess),
    exitFailure,
  )
import System.Process
  ( CreateProcess (delegate_ctlc, env),
    createProcess,
    proc,
    waitForProcess,
  )
import qualified Data.String.ToString as T

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
  let allArgs = ["--context", context config, "-f", "docker-compose.yml", "-f", "docker-compose.production.yml"] <> args
  callProcessWithEnv (allEnvs config) "docker-compose" (Prelude.map T.unpack allArgs)
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
  writeFileIfAbsent configPathTxt (T.decodeUtf8 . B.concat . LB.toChunks $ json)
  writeFileIfAbsent "docker-compose.yml" (initialDockerComposeYml config)
  writeFileIfAbsent "docker-compose.production.yml" (initialDockerComposeProductionYml config)
  writeFileIfAbsent "docker-compose.development.yml" (initialDockerComposeDevelopmentYml config)
  writeFileIfAbsent ".gitignore" ".env.production"
  writeFileIfAbsent ".env.production" ""
  writeFileIfAbsent ".env.development" ""

writeFileIfAbsent :: T.Text -> T.Text -> IO ()
writeFileIfAbsent path text = do
  fileExists <- isFile (fromText path)
  if fileExists
    then T.putStrLn (path <> " already exists")
    else T.writeFile (T.toString path) text

type Tag = T.Text

release :: Config -> Tag -> ConfigPath -> IO ()
release config tag configPath = do
  let updatedConfig = config {tag = tag}
      json = A.encodePretty updatedConfig
  LB.writeFile (toString configPath) json
  forM_ (M.toList $ images config) $ \(imageName, dockerfile) -> do
    let imageNameWithTag = fullQualifiedImageName config imageName <> ":" <> tag
    callProcessWithEnv M.empty "docker" ["buildx", "build", ".", "-f", dockerfile, "-t", imageNameWithTag, "--platform", "linux/amd64"]
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
  let _proj = projectName config
  in [text|
    version: '3'
    services:
      web:
        command: TODO
        restart: always
        links:
          - postgres
        depends_on:
          - postgres
      postgres:
        image: postgres:12-alpine
        restart: always
        volumes:
          - ${_proj}_pg_data:/var/lib/postgresql/data 

    volumes:
      ${_proj}_pg_data:
        external: true
  |]

initialDockerComposeProductionYml config =
  let _imageName = fullQualifiedImageName config "web"
  in
  [text|
    services:
      web:
        image: $_imageName:$${TAG}
        ports:
          - "127.0.0.1:$${PORT:?err}:3000"
        env_file: .env.production
      postgres:
        env_file: .env.production
  |]

initialDockerComposeDevelopmentYml config =
  [text|
    services:
      web:
        build: .
        ports:
          - "127.0.0.1:3000:3000"
        env_file: .env.development
      postgres:
        env_file: .env.development
  |]
