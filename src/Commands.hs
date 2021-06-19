module Commands (deploy, dockerCompose, initialize, release, Args, ProjectName, Tag) where

import Config (Config)
import qualified Data.Text as T

deploy :: Config -> IO ()
deploy config = return ()

type Args = [T.Text]

dockerCompose :: Config -> Args -> IO ()
dockerCompose config args = return ()

type ProjectName = T.Text

initialize :: ProjectName -> IO ()
initialize projectName = return ()

type Tag = T.Text

release :: Config -> Tag -> IO ()
release config tag = return ()