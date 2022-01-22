module Commands (deploy, gracefulDeploy, dockerCompose, initialize, release, Args, ProjectName, Tag) where

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
import System.IO.Temp (emptySystemTempFile)
import qualified GHC.IO.Handle as T
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Aeson as Yaml
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap

import System.Directory (removeFile)

type ProjectName = T.Text

type ConfigPath = T.Text

type Context = T.Text

type Registry = T.Text

type Args = [T.Text]

deploy :: Config -> [T.Text] -> IO ()
deploy config services = do
  dockerCompose config $ ["up", "-d"] <> services

dockerCompose :: Config -> Args -> IO ()
dockerCompose config args = do
  let allArgs = ["--context", context config, "-f", "docker-compose.yml", "-f", "docker-compose.production.yml"] <> args
  callProcessWithEnv (allEnvs config) "docker-compose" (Prelude.map T.unpack allArgs)
  return ()

gracefulDeploy :: Config -> [T.Text] -> IO ()
gracefulDeploy config services = do
  dockerComposeBackup config $ ["--project-directory", ".", "up", "-d"] <> map (<> "_backup") services
  dockerCompose config $ ["up", "-d"] <> services
  dockerCompose config $ ["up", "-d", "--remove-orphans"] <> services

dockerComposeBackup :: Config -> Args -> IO()
dockerComposeBackup config args = do
  let
    createBackupFile originalFile = do
      originalData <- Yaml.decodeFileThrow originalFile
      let
        Just originalDataValue = originalData :: Maybe Aeson.Value
        for = flip map
        Aeson.Object topLevelObject = originalDataValue
        modifiedDataValue = Aeson.Object . HashMap.fromList $ for (HashMap.toList topLevelObject) $ \(name1, value1) -> if name1 /= "services"
          then (name1, value1)
          else case value1 of
            Aeson.Object object1 -> (
              name1,
              Aeson.Object . HashMap.fromList $ for (HashMap.toList object1) $ \(name2, value2) -> if name2 `notElem` (M.keys . images $ config)
                then (name2, value2)
                else (name2 <> "_backup", value2)
              )
            _ -> error ("unexpected service: " <> T.unpack name1)
      modifiedFile <- emptySystemTempFile originalFile
      Aeson.encodeFile modifiedFile modifiedDataValue
      return modifiedFile
  dcFileName <- createBackupFile "docker-compose.yml"
  dcProdFileName <- createBackupFile "docker-compose.production.yml"
  let
    backupConfig = config { port = T.pack . show $ (read . T.unpack . port $ config) + 10000 }
    allArgs = ["--context", context backupConfig, "-f", T.pack dcFileName, "-f", T.pack dcProdFileName] <> args
  callProcessWithEnv (allEnvs backupConfig) "docker-compose" (Prelude.map T.unpack allArgs)
  removeFile dcFileName
  removeFile dcProdFileName

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
