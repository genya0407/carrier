module Commands (deploy, dockerCompose, initialize, release, Args, ProjectName, Tag) where

import Config (Config (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import Data.Bifunctor
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
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

deploy :: Config -> IO ()
deploy config = return ()

type Args = [T.Text]

callProcessWithEnv :: M.Map T.Text T.Text -> FilePath -> [String] -> IO ()
callProcessWithEnv envs cmd args = do
  let p = (proc cmd args) {delegate_ctlc = True, env = Just (map (bimap T.unpack T.unpack) $ M.toList envs)}
  (_, _, _, handle) <- createProcess p
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure r -> exitFailure

dockerCompose :: Config -> Args -> IO ()
dockerCompose config args = do
  outerEnvList <- fmap (map (bimap T.pack T.pack)) getEnvironment
  let defaultEnvs = M.fromList [("PORT", port config), ("TAG", tag config)]
      outerEnvs = M.fromList outerEnvList
      allEnvs = outerEnvs `M.union` defaultEnvs `M.union` environments config
      allArgs = ["--context", context config] <> args
  callProcessWithEnv allEnvs "docker-compose" (map T.unpack allArgs)
  return ()

type ProjectName = T.Text

type ConfigPath = T.Text

type Context = T.Text

initialize :: ProjectName -> ConfigPath -> Context -> IO ()
initialize projectName configPathTxt context = do
  let configPath = fromText configPathTxt
      config = Config {projectName = projectName, port = "3000", tag = "v1.0,0", context = context, environments = M.empty}
      json = A.encodePretty config
  configFileExists <- isFile configPath
  if configFileExists
    then T.putStrLn ("'" <> configPathTxt <> "' already exists.") >> exitFailure
    else B.writeFile (encodeString configPath) json

type Tag = T.Text

release :: Config -> Tag -> IO ()
release config tag = return ()