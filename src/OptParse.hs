module OptParse (ConfigPath, Command (..), parseCommand) where

import qualified Commands as CM
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Options.Applicative

parseCommand :: IO Command
parseCommand = do
  execParser opts
  where
    opts =
      info
        (cmdParser <**> helper)
        ( fullDesc
            <> progDesc "Carrier management tool"
        )

configOption :: Parser T.Text
configOption = T.pack <$> strOption (long "config" <> short 'c' <> metavar "FILE" <> value "carrier.json" <> help "Configuration file")

initializeParser :: Parser Command
initializeParser = InitializeOpts <$> strArgument (metavar "NAME")

deployParser :: Parser Command
deployParser = DeployOpts <$> configOption

dockerComposeParser :: Parser Command
dockerComposeParser = DockerComposeOpts <$> configOption <*> some (strArgument (metavar "ARGS..."))

releaseParser :: Parser Command
releaseParser = ReleaseOpts <$> configOption <*> strArgument (metavar "TAG")

cmdParser :: Parser Command
cmdParser =
  hsubparser
    ( command "init" (info initializeParser (progDesc "Initialize carrier for this project"))
        <> command "deploy" (info deployParser (progDesc "Exec deployment"))
        <> command "docker-compose" (info dockerComposeParser (progDesc "Exec docker-compose command with given arguments"))
        <> command "release" (info releaseParser (progDesc "Create git tag, build docker image, push it to registry"))
    )

type ConfigPath = T.Text

data Command = InitializeOpts CM.ProjectName | DeployOpts ConfigPath | DockerComposeOpts ConfigPath CM.Args | ReleaseOpts ConfigPath CM.Tag deriving (Show)
