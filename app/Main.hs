module Main where

import qualified Commands as CM
import Config (Config)
import qualified Data.Aeson as A
import qualified Data.Text as T
import OptParse (Command (..), parseCommand)
import System.Exit (exitFailure)

loadConfig :: T.Text -> IO Config
loadConfig path = do
  let pathString = T.unpack path
  configMaybe <- A.decodeFileStrict pathString
  case configMaybe of
    Just config -> return config
    Nothing -> do
      putStrLn ("Failed to parse '" ++ pathString ++ "'")
      exitFailure

main :: IO ()
main = do
  cmd <- parseCommand
  case cmd of
    InitializeOpts projectName configPath context -> CM.initialize projectName configPath context
    DeployOpts configPath -> loadConfig configPath >>= CM.deploy
    DockerComposeOpts configPath args -> loadConfig configPath >>= \cfg -> CM.dockerCompose cfg args
    ReleaseOpts configPath tag -> loadConfig configPath >>= \cfg -> CM.release cfg tag
